(ns com.jimrthy.substratum.core
  "Core database functionality

There might be some justification for splitting some of these
pieces from platform. But anything I don't move into there should
probably get moved into, say, _impl"
  (:require [clojure.spec.alpha :as s]
            [com.jimrthy.substratum.log :as log]
            [com.jimrthy.substratum.util :as util]
            [datomic.api :as d])
  (:import [datomic Datom]
           [datomic.db Db DbId]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specs
;;; N.B. Database schema definitions belong in admin.
;;; This is definitely for Prismatic-style schema and
;;; the way it interacts with Stuart Sierra Components

(s/def ::protocol
  ;; TODO: Add the rest
  #{::free ::ram ::sql})

(s/def ::db-name string?)
(s/def ::base-uri-description (s/keys :req [::db-name ::protocol]))

(s/def ::in-memory-uri-description ::base-uri-description)

(s/def ::port (s/and integer?
                     (complement neg?)
                     #(< % 65536)))
(s/def ::describe-uri-with-port (s/merge ::base-uri-description
                                         (s/keys ::port ::host)))

(s/def ::free-uri-description ::describe-uri-with-port)

(s/def ::sql-driver #{::postgres})
(s/def ::user string?)
(s/def ::password string?)
;; This depends on the JDBC part of the connection.
(s/def ::sql-uri-description
  (s/merge ::describe-uri-with-port
           (s/keys :opt [::sql-driver ::user ::password])))

(defmulti uri-types :protocol)
(defmethod uri-types ::free
  [_]
  ::free-uri-description)
(defmethod uri-types ::ram
  [_]
  ::in-memory-uri-description)
(defmethod uri-types ::sql
  [_]
  ::sql-uri-description)
(s/def ::possible-uri-descriptions (s/multi-spec uri-types :protocol))

(defmulti build-connection-string-types ::protocol)
(defmulti build-connection-string ::protocol)
(s/def ::connection-string-builder-types (s/multi-spec build-connection-string-types ::protocol))

(defmulti disconnect-type ::protocol)
(defmulti disconnect ::protocol)
(s/def ::disconnect-types (s/multi-spec disconnect-type ::protocol))

;;; TODO: Surely these have already been captured somewhere

;; Q: What's a good way to specify that this might have
;; a length of 2-4?
;; Actually, this is defined to be 1+ clauses, which can be lots and lots of things.
;; So this over-simplification has been broken from the beginning.
(s/def ::where-clause (s/or :ea (s/tuple  symbol? symbol?)
                            :eav (s/tuple symbol? symbol? symbol?)
                            :eavt (s/tuple symbol? symbol? symbol? symbol?)))

(s/def ::find (s/coll-of symbol?))
(s/def ::where ::where-clause)
(s/def ::in (s/coll-of symbol?))
(s/def ::datomic-query (s/keys :req-un [::find ::where]
                               :opt-un [::in]))

(s/def :db/id (s/or :id #(instance? DbId %)
                    :keyword keyword?))
(s/def ::base-transaction (s/keys :req [:db/id]))

;; Really just a bunch of attribute/value pairs
(s/def ::upsert-transaction
  (s/merge ::base-transaction
           (s/map-of keyword? any?)))

;;; Q: What's a good way to represent these?
;; UpsertTransactions get translated into these,
;; which belies the name.
;; Note that retractions cannot be represented as a map
;; Q: Is this really supposed to be a coll-of?
(s/def ::retract-txn (s/tuple #{:db/add :db/retract}
                              #{integer? keyword?}
                              any?))

(s/def ::db-before #(instance? Db %))
(s/def ::db-after #(instance? Db %))
(s/def ::tx-data (s/coll-of #(instance? Datom %)))
(s/def ::temp-ids (s/map-of integer? integer?))
(s/def ::transaction-result (s/keys :req-un [::db-before ::db-after ::tx-data ::temp-ids]))

(s/def ::description ::possible-uri-descriptions)
;; TODO: Is there something like a regex that would be useful for this?
(s/def ::connection-string string?)

(s/def ::db-url (s/keys :opt [::connection-string
                              ::connection]
                        :req [::description]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal

(defmethod build-connection-string-types ::free
  [_]
  ::free-uri-description)
(defmethod build-connection-string ::free
  [{:keys [::db-name ::host ::port]
    :or {host "localhost"
         ;; TODO: Verify that this is the default
         port 4334}}]
  (str "datomic:free://" host ":" port "/" db-name))

(defmethod build-connection-string-types ::ram
  [_]
  ::in-memory-uri-description)
(defmethod build-connection-string ::ram
  [{:keys [::db-name] :as args}]
  (when-not db-name
    (throw (ex-info "Missing database name" args)))
  (str "datomic:mem://" db-name))

(s/fdef sql-driver
        :args (s/cat :driver-key ::sql-drivers)
        :ret string?)
(defn sql-driver
  "Return the string representation of a JDBC driver"
  [driver-key]
  ;; Q: ms?
  ;; Note that this should really be much more interesting,
  ;; with an entire slew of options.
  (let [ms {::postgres "postgresql"}]
    (ms driver-key)))

(defmethod build-connection-string-types ::sql
  [_]
  ::sql-uri-description)
(defmethod build-connection-string ::sql
  [{:keys [::db-name ::port ::driver ::user ::password ::server]
    :or {port 5432
         user "datomic"
         password "datomic"
         server "localhost"}
    :as descr}]
  (when-not driver
    (throw (ex-info "missing-driver" {::description descr})))
  ;; Next construct is weird because I've shadowed a builtin
  (str "datomic:sql://" db-name "?jdbc:" (sql-driver driver)
       "://" server ":" port "/datomic?user="
       user "&password=" password))

(defmethod disconnect-type ::ram
  [_]
  ::in-memory-uri-description)
(defmethod disconnect ::ram
  [descr]
  ;; We really don't want to keep a reference around to these
  (let [cxn-str (build-connection-string descr)]
    (d/delete-database cxn-str)))

(s/fdef general-disconnect
        :args (s/cat :uri ::possible-uri-descriptions)
        ;; Q: What does this return?
        :ret any?)
(defn general-disconnect
  "Generally don't want to delete the database

  This should be done when the entire process loses interest
  in the connection.
  Its results are async.

I'm mainly including this on the theory that I might want to switch
to a different connection during a reset, and multiple connections
really aren't legal (and probably won't work).

Q: Are those assumptions about multiple connections still true?"
  [descr]
  (-> descr build-connection-string d/connect d/release))

(defmethod disconnect-type ::sql
  [_]
  ;; The original schema for this specified possible-uri-descriptions.
  ;; Q: Why that instead of this?
  ::sql-uri-description)
(defmethod disconnect ::sql
  [descr]
  (general-disconnect descr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

(s/fdef start!
        :args (s/cat :logger ::log/entries
                     :connection-description ::description)
        :ret (s/tuple ::db-url ::log/entries))
(defn start!
    "Main point is to verify that we can connect
Although this also serves to create the database
if it doesn't already exast and cache the connection"
  [logger description]
  (let [log-messages (log/debug logger
                                ::lifecycle.start
                                "Starting a database connection"
                                description)
        connection-string (build-connection-string description)]
    (let [log-messages
          (if (d/create-database connection-string)
            (log/warn logger
                      ::lifecycle.start
                      "Created new database")
            log-messages)]
      [{::connection (d/connect connection-string)
        ::connection-string connection-string
        ::description description}
       log-messages])))

(s/fdef q
        :args (s/cat :uri ::connection-string
                     :query ::datomic-query)
        ;; Q: What do I know about the actual return results?
        ;; A: Well, it's actually a seq of seqs. Where the inner seq matches the query's :find
        ;; clause. That seems worth encoding formally.
        :ret (s/coll-of any?))
(defn q
  "Convenience function for querying the database.
  Probably shouldn't actually use very often.

  In general, we should probably be running queries against database values
  using d/q. But this approach does save some typing"
  [uri query]
  (d/q query (-> uri d/connect d/db)))

(s/fdef pretend-upsert!
        :args (s/cat :uri ::connection-string
                     :txns (s/coll-of ::upsert-transaction))
        :ret ::transaction-result)
(defn pretend-upsert!
  "Re-bind upsert! to this for experimentation
or unit testing

Then again, that would mean making it dynamic, which
seems like a bad idea. If nothing else, I think it
has noticeable performance impact because of the
var lookup"
  [uri txns]
  (let [conn (d/connect uri)
        database-value (d/db conn)]
    (d/with database-value txns)))

(s/fdef upsert!
        :args (s/cat :uri ::connection-string
                     :txns (s/coll-of ::upsert-transaction))
        :ret ::transaction-result)
(defn upsert!
  [uri txns]
  (let [conn (d/connect uri)]
    @(d/transact conn txns)))
