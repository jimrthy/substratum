(ns com.jimrthy.substratum.core
  "Core database functionality"
  (:require [com.stuartsierra.component :as component]
            [datomic.api :as d]
            [com.jimrthy.substratum.util :as util]
            [hara.event :refer [raise]]
            [schema.core :as s]
            [taoensso.timbre :as log])
  (:import [datomic Datom]
           [datomic.db Db DbId]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schema
;;; N.B. Database schema definitions belong in admin.
;;; This is definitely for Prismatic-style schema and
;;; the way it interacts with Stuart Sierra Components

(defn protocols []
  ;; TODO: Add the rest
  (s/enum :free :ram :sql))

(defn sql-drivers
  []
  (s/enum :postgres))

(defn base-uri-description
  []
  {:db-name s/Str
   :protocol (protocols)})

(def in-memory-uri-description base-uri-description)

(defn describe-uri-with-port
  []
  (assoc (base-uri-description)
         :port s/Int
         :host s/Str))

(def free-uri-description describe-uri-with-port)

(defn sql-uri-description
  "This all depends on the JDBC part of the connection."
  []
  (into (describe-uri-with-port)
        {(s/optional-key :driver) (sql-drivers)
         (s/optional-key :user) s/Str
         (s/optional-key :password) s/Str}))

(defn possible-uri-descriptions
  []
  (let [protocol-p (fn [expected actual]
                     (= expected (:protocol actual)))]
    (s/conditional (partial protocol-p :free) (free-uri-description)
                   (partial protocol-p :ram) (in-memory-uri-description)
                   (partial protocol-p :sql) (sql-uri-description))))

(defmulti build-connection-string :protocol)
(defmulti disconnect :protocol)

;;; TODO: Come up with a better/less contentious name
(s/defrecord URL [description :- (possible-uri-descriptions)
                  connection-string :- s/Str]
  component/Lifecycle
  (start
   [this]
   "Main point is to verify that we can connect
Although this also serves to create the database
if it doesn't already exast and cache the connection"
   (comment) (log/debug "Starting up the URL. Description: " (util/pretty description)
                        "with keys:" (keys description))
   (let [connection-string (build-connection-string description)]
     (when (d/create-database connection-string)
       (log/warn "Created new database"))
     (d/connect connection-string)
     (assoc this :connection-string connection-string)))
  (stop
   [this]
   (disconnect description)
   ;; Can't just dissoc...that would return
   ;; an ordinary map that couldn't be started
   ;; At least, that seems to be what I'm picking up
   ;; from the mailing list
   (assoc this :connection-string nil)))

;;; Printing helpers.
;;; TODO: These really belong in their own namespace.
;;; Or, at least, not in this one
;;; Q: Should they really be calling print??

(defmethod io.aviso.exception/exception-dispatch URL
  [url]
  (print "<#URL" (:connection-string url) ">"))

(defmethod io.aviso.exception/exception-dispatch DbId
  [db-id]
  ;; This is super cheesy. But I need something to keep
  ;; my error reporting about it from throwing another exception
  (print "<#DbId" (str db-id) ">"))

(defmethod io.aviso.exception/exception-dispatch schema.core.Either
  [either]
  ;; It's tough to fathom that this doesn't have a good default printer
  (print "(schema.core/either" (:schemas either) ")"))

;;; TODO: Surely these have already been captured somewhere

;; Q: What's a good way to specify that this might have
;; a length of 2-4?
(defn where-clause []
  [s/Symbol])

(defn datomic-query []
  {:find [s/Symbol]
   (s/optional-key :in) [s/Symbol]
   :where [[(where-clause)]]})

(defn BaseTransaction []
  {:db/id (s/either DbId s/Keyword)})

;; Really just a bunch of attribute/value pairs
(defn UpsertTransaction []
  (into (BaseTransaction) {s/Keyword s/Any}))

;;; Q: What's a good way to represent these?
;; UpsertTransactions get translated into these.
;; Note that retractions cannot be represented as a map
(defn RetractTxn []
  [(s/one (s/enum :db/add :db/retract) "Action")
   ;; s/either is deprecated
   ;; TODO: Switch to s/conditional
   (s/one (s/either s/Int s/Keyword) "Which entity")
   s/Any])

(defn TransactionResult []
  {:db-before Db
   :db-after Db
   :tx-data [Datom]
   :temp-ids {s/Int s/Int}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal

(s/defmethod build-connection-string :free :- s/Str
  [{:keys [db-name host port]
    :or {host "localhost"
         ;; TODO: Verify that this is the default
         port 4334}}]
  (str "datomic:free://" host ":" port "/" db-name))

(s/defmethod build-connection-string :ram :- s/Str
  [{:keys [db-name] :as args}]
  (when-not db-name
    (throw (ex-info "Missing database name" args)))
  (str "datomic:mem://" db-name))

(s/defn sql-driver :- s/Str
  "Return the string representation of a JDBC driver"
  [driver-key :- sql-drivers]
  (let [ms {:postgres "postgresql"}]
    (ms driver-key)))

(s/defmethod build-connection-string :sql :- s/Str
  [{:keys [db-name port driver user password server]
    :or {port 5432
         user "datomic"
         password "datomic"
         server "localhost"}
    :as descr} :- (sql-uri-description)]
  (when-not driver
    (raise :missing-driver))
  ;; Next construct is weird because I've shadowed a builtin
  (str "datomic:sql://" db-name "?jdbc:" (sql-driver driver)
       "://" server ":" port "/datomic?user="
       user "&password=" password))

(s/defmethod disconnect :ram
  [descr :- (in-memory-uri-description)]
  ;; We really don't want to keep a reference around to these
  (let [cxn-str (build-connection-string descr)]
    (d/delete-database cxn-str)))

(s/defn general-disconnect
  "Generally don't want to delete the database

  This should be done when the entire process loses interest
  in the connection.
  Its results are async.

I'm mainly including this on the theory that I might want to switch
to a different connection during a reset, and multiple connections
really aren't legal (and probably won't work)."
  [descr :- (possible-uri-descriptions)]
  (-> descr build-connection-string d/connect d/release))

(s/defmethod disconnect :sql
  [descr :- (possible-uri-descriptions)]
  (general-disconnect descr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

(s/defn uri-ctor :- URL
  [description :- {:description (possible-uri-descriptions)}]
  (map->URL description))

(s/defn q :- [s/Any]
  "Convenience function for querying the database.
Probably shouldn't actually use very often.

In general, we should probably be running queries against database values
using d/q. But this approach does save some typing"
  [query :- datomic-query
   uri :- s/Str]
  (d/q query (-> uri d/connect d/db)))

(s/defn pretend-upsert! :- TransactionResult
  "Re-bind upsert! to this for experimentation
or unit testing

Then again, that would mean making it dynamic, which
seems like a bad idea. If nothing else, I think it
has noticeable performance impact because of the
var lookup"
  [uri :- s/Str
   txns :- [UpsertTransaction]]
  (let [conn (d/connect uri)
        database-value (d/db conn)]
    (d/with database-value txns)))

(s/defn upsert! :- TransactionResult
  [uri :- s/Str
   txns :- [UpsertTransaction]]
  (let [conn (d/connect uri)]
    @(d/transact conn txns)))
