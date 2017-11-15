(ns com.jimrthy.substratum.installer
  "Really just specs and wrappers around conformity and datomic-schema

It translates from a high-level description that makes sense to me into what they need

Note that the main point is to install the pieces defined in Antonio Andrade's
talk about datomic data platforms, as translated in the schema ns.

Actually, the main point is to use that data platform. This is another step
in that direction."
  (:require [clojure.spec.alpha :as s]
            [com.jimrthy.substratum.core :as substratum]
            [com.jimrthy.substratum.log :as log]
            [datomic.api :as d]
            [datomic-schema.schema :refer [defdbfn
                                           fields]
             :as yuppie-schema]
            [com.jimrthy.substratum.core :as db]
            [com.jimrthy.substratum.log :as log]
            [com.jimrthy.substratum.schema :as schema]
            [com.jimrthy.substratum.util :as util]
            [io.rkn.conformity :as conformity])
  (:import [clojure.lang ExceptionInfo]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specs

;; TODO: Be more restrictive
(s/def ::schema-resource-name string?)
(s/def ::uri :com.jimrthy.substratum.core/possible-uri-descriptions)
;; Q: Are there any restrictions on this?
(s/def ::partition-name string?)
(s/def ::database-definition (s/keys :req-un [:com.jimrthy.substratum.core/uri
                                              ::partition-name]))
;; A: Until there's something useful to do with start/stop,
;; just use a plain hashmap instead of a real defrecord Component.
;; It's tempting to make (start) convert the uri to a connection-string.
;; That temptation seems like a mistake.
(s/def ::database-schema (s/merge ::database-definition
                                  (s/keys :req-un [::schema-resource-name])))
;; The parameters to create that
(s/def ::opt-database-schema (s/keys :opt-un [::schema-resource-name
                                              :com.jimrthy.substratum.core/uri]))

(s/def ::uniqueness #{:db.unique/identity ; attempts to insert dupe value for different entity will fail
                      :db.unique/value})  ; attempts to insert dupe value for entity w/ tempid will merge existing entity

(s/def ::value-types #{:db.type/bigdec
                       :db.type/bigint
                       :db.type/boolean
                       :db.type/bytes
                       :db.type/double
                       :db.type/float
                       :db.type/instant
                       :db.type/keyword
                       :db.type/long
                       :db.type/ref
                       :db.type/string
                       :db.type/uri
                       :db.type/uuid})

(s/def ::cardinality-options #{:db.cardinality/one
                               :db.cardinality/many})

(s/def :db/ident keyword?)
(s/def :db/cardinality ::cardinality-options)
(s/def :db/valueType ::value-types)
(s/def :db.install/_attribute #(= :db.part/db %))
(s/def :db/doc string?)
(s/def :db/fulltext boolean?) ; Generate an eventually consistent fulltext search
(s/def :db/index boolean?)
(s/def :db/isComponent boolean?) ; ref attributes become sub-components
(s/def :db/no-history boolean?)
(s/def :db/unique ::uniqueness)

;;; For adding schema
(s/def ::schema-transaction (s/merge :com.jimrthy.substratum.core/base-transaction
                                     (s/keys :req [:db/ident
                                                   :db/cardinality
                                                   :db/valueType
                                                   ;; TODO: We could also do alterations
                                                   :db.install/_attribute]
                                             :opt [:db/doc
                                                   :db/fulltext
                                                   :db/index
                                                   :db/isComponent
                                                   :db/no-history
                                                   ;; FIXME: Before I started porting, this was :db.unique
                                                   ;; Which I'm about 90% certain was wrong.
                                                   ;; But it was working (for a very loose, not-really-
                                                   ;; implemented definition of "working"), so I'm
                                                   ;; leery about changing this without double-checking.
                                                   ;; But the previous version quit compiling, making
                                                   ;; this that much more likely
                                                   :db/unique])))

(s/def :db.install/_partition #(= :db.part/db %))
;; This could be done as two steps in one transaction...but why?
(s/def ::partition-transaction (s/merge :com.jimrthy.substratum.core/base-transaction
                                        (s/keys :req [:db/ident
                                                      :db.install/_partition])))

(s/def ::individual-txn (s/or :schema ::schema-transaction
                              :parts ::partition-transaction
                              :upsert :com.jimrthy.substratum.core/upsert-transaction
                              :retract :com.jimrthy.substratum.core/retract-txn))
(s/def ::txn-sequence (s/coll-of ::individual-txn))

;; Really just a sequence of names
(s/def ::part-txn-descr-seq (s/coll-of string?))
(s/def ::attribute-options (s/cat :primitive-type ::primitive-type
                                  ;; Punt on this one for now
                                  :options (s/coll-of any?)))
(s/def ::type-description (s/map-of symbol? ::attribute-options))
;; Symbol that describes the type, mapped to a tuple of the primitive type
;; (as a keyword) and an optional set of options (most importantly, the doc string)
(s/def ::attr-type-txn
  (s/map-of symbol? ::type-description))

;;; Transaction that builds an individual attribute
;;;
;;; These are almost value-types,
;;; but YuppieChef adds the namespace for us
(s/def ::attr-txn-descr (s/map-of symbol? (s/coll-of ::attribute-options)))
(s/def ::attr-txn-descr-seq (s/coll-of ::attr-txn-descr))
(s/def ::partitions ::part-txn-descr-seq)
(s/def ::attribute-types (s/coll-of ::attr-type-txn))
(s/def ::attributes ::attr-txn-descr-seq)

;; In the original, I had a comment that it's just a
;; sequence of names, and it was defined as (s/coll-of string?)
;; That was definitely wrong.
;; However, that doesn't mean I don't have something that needs
;; that definition.
(s/def ::txn-dscr-seq (s/keys :req [::partitions ::attribute-types ::attributes]))

;; Q: Are these next two even vaguely close to reality?
;; I'm really trying to reverse-engineer what's going on inside here
(s/def ::structure ::txn-sequence)
(s/def ::data ::txn-sequence)
(s/def ::platform-txns (s/keys :req [::structure ::data]))

(s/def ::norm-name (s/or :string string?
                         :keyword keyword?))
(s/def ::tx-index integer?)
;;; Q: What is this?
(s/def ::tx-result any?)
(s/def ::conformation (s/keys :req-un [::norm-name
                                       ::tx-index
                                       ::tx-result]))
(s/def ::conformation-sequence (s/coll-of ::conformation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal

;; TODO: ^:always-validate
;; TODO: Refactor/rename to do-schema-installation!
(s/fdef do-schema-installation
        :args (s/cat :uri :com.jimrthy.substratum.core/uri
                     :partition-name string?
                     :transactions ::txn-seq)
        ;; Q: What does this return?
        ;; A: Well, last time I checked (and according to the
        ;; comments below), nil on success.
        ;; According to frereth-server, ::conformation-sequence
        ;; It seems like it's changed from one to the other.
        ;; Q: What does it return now?
        :ret [any? ::log/entries])
(defn do-schema-installation
  "Add schema/partition"
  [logger
   uri
   partition-name
   transactions]
  (d/create-database uri)
  (let [conn (d/connect uri)
        partition-key (keyword partition-name "base-schema")
        ;; Q: Worth generating this part using either schematode
        ;; or yuppiechef's library instead?

        ;; Need a better way to hide the API.
        ;; Since any given conformation will only be applied once per
        ;; name, the thing calling this should really be setting this up.
        ;; And, honestly, it's not like it's asking a lot to have them
        ;; wrap the transactions into a norms-map shape.
        ;; Or maybe I shouldn't be trying to hide it in the first place.
        norms-map {partition-key {:txes [(vec transactions)]}}
        logger (log/info
                logger
                ::conforming
                "Doing it"
                {::connection conn
                 ::uri uri
                 ::norms-map norms-map
                 ::partition partition-name})]
    ;; Q: Which of these do I want?
    (let [result (conformity/ensure-conforms conn norms-map)
          original (conformity/ensure-conforms conn norms-map [partition-key])
          logger (log/debug logger ::do-schema "ensure-conforms returned" result)]
      ;; Returns nil on success
      [result logger])))

(s/fdef load-transactions-from-resource!
        :args (s/cat :logger ::log/log-entries
                     :resource-name string?)
        :ret [::txn-dscr-seq ::log/log-entries])
(defn load-transactions-from-resource!
  [resource-name
   logger]
  (let [logger
        (log/debug logger
                   ::load-txns
                   "Getting ready to load schema transaction from resource"
                   {::name resource-name})])
  [(util/load-resource resource-name) logger])

(defn schema-black-magic
  "Converts a pair of (attribute name, field descriptions) into something datomic-schema can expand.

Really just refactored out of a map description"
  [logger
   [attr field-descrs]]
  (let [logger (log/debug logger ::black-magic
                          "Expand attribute/field descriptions"
                          {::attribute-name attr
                           ::description field-descrs})]
    ;; Under the covers, Yuppiechef's schema macro just
    ;; calls schema*.
    (yuppie-schema/schema* (name attr)
                           {:fields (reduce (fn [[logger
                                                  acc] [k v]]
                                              (comment (.debug logger "Setting up field" k "with characteristics" v))
                                              (let [result
                                                    (assoc acc (name k)
                                                           (if (= (count v) 1)
                                                             ;; If there isn't an option set,
                                                             ;; append one to a vector of the field.
                                                             ;; Because we might have a lazy seq here.
                                                             (do
                                                               (comment (.debug logger "Adding default empty set"))
                                                               (conj (vec v) #{}))
                                                             (if (= (count v) 2)
                                                               v
                                                               (throw (ex-info (str "Bad field description\n"
                                                                                    v
                                                                                    " => "
                                                                                    k) {:illegal-field-description v
                                                                                        :field-id k})))))]
                                                [logger result]))
                                            [logger {}]
                                            field-descrs)})))

;;; Q: What does this return?
(s/fdef expand-schema-descr
        :args (s/cat :logger ::log/entries
                     :descr ::attr-txn-descr-seq)
        :ret (s/tuple any? ::log/log-entries))
(defn expand-schema-descr
  "Isolating a helper function to start expanding attribute descriptions into transactions"
  [logger descr]
  (let [logger
        (log/info logger
                  ::schema.expansion
                  "Expanding Schema Description"
                  descr)]
    (map schema-black-magic logger descr)))

(defn expanded-descr->schema
  "Take the output of expand-schema-descr (which should be identical
to the output of a seq of Yuppiechef's schema macro) and run it
through generate-schema to generate actual transactions"
  [logger attrs]
  (let [logger
        (log/debug logger
                   ::dscr->schema
                   "calling generate-schema"
                   attrs)]
    [(yuppie-schema/generate-schema attrs {:index-all? true}) logger]))

(s/fdef expand-txn-descr
        :args (s/cat :logger ::log/entries
                     :descr ::txn-descr-seq)
        :ret (s/tuple ::platform-txns ::log/entries))
(defn expand-txn-descr
  "Convert from a slightly-more-readable high-level description
to the actual datastructure that datomic uses"
  [logger descr]
  (let [logger (log/info logger ::expand.txn-dscr
                         "Expanding Transaction Description"
                         descr)])
  (let [parts (map yuppie-schema/part (:partitions descr))
        [attrs logger] (expand-schema-descr logger (:attribute-types descr))
        [generated-schema logger] (expanded-descr->schema logger attrs)
        entities (:attributes descr)]
    [{::structure (concat (yuppie-schema/generate-parts parts)
                         generated-schema)
      ::data entities}
     logger]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

;;; It's generally better to just install your schema from a resource
;;; definition.
;;; Recompiling code to modify database definitions went out of style
;;; back in the 70's.
;;; But doing it this way will probably always be more convenient from
;;; the REPL.
;;; And the main point behind this entire library is to set up the
;;; platform defined in your schema namespace so you (or, really,
;;; your customers) can use that as the foundation for the data
;;; they care about.

;;; Although there are really two pieces there:
;;; 1. Install the low-level platform schema
;;; 2. Install the higher-level data modelling schema for individual apps
;;; that's built on top of that platform

;; TODO: ^:always-validate
(s/fdef install-schema!
        :args (s/cat :logger ::log/entries
                     :uri-description ::substratum/uri-description
                     ;; Q: Does this make any sense?
                     ;; It's a nice shortcut, but ::tx-dscr-seq
                     ;; specifically includes a ::partitions key for
                     ;; partition-creation transactions.
                     ;; Or maybe I'm tangling my abstraction layers.
                     ;; Is this the partition where attributes and
                     ;; data types should live, as opposed to the
                     ;; partitions the actual data should occupy?
                     :partition-name string?
                     :tx-description ::txn-dscr-seq)
        ;; Q: What does this return?
        :ret [any? ::log/entries])
(defn install-schema!
  [logger uri-description partition-name tx-description]
  (let [uri (db/build-connection-string uri-description)
        logger (log/debug logger
                          ::do!
                          "Expanding high-level schema transaction description"
                          tx-description)
        [{:keys [::structure ::data]} logger] (expand-txn-descr logger tx-description)
        logger (log/debug logger
                          ::do!
                          "Setting up schema"
                          {::structure structure
                           ::uri uri})]
        (try
          (if (s/valid? ::txn-sequence structure)
            (let [[success logger]
                  (reduce (fn [[logger acc]
                               step]
                            (let [[outcome logger] (do-schema-installation logger uri partition-name structure)]
                              [(conj acc outcome) logger]))
                          [logger []]
                          structure)]
              ;; This has to happen as a Step 2:
              ;; We can't assign attributes to entities until
              ;; after the transaction that generates the schema
              [(db/upsert! uri data) logger])
            (throw (ex-info (str "Invalid transaction sequence:\n"
                                 (util/pretty
                                  (s/explain-data ::txn-sequence structure))
                                 "Installing schema based on\n"
                                 (util/pretty structure)
                                 "\nwhich has" (count structure) "members")
                            {::problem-tx tx-description
                             ::problem-struct structure
                             ::tx-dscr tx-description
                             ::uri uri
                             ::uri-description uri-description}))))))

(s/fdef install-platform!
        :args (s/cat :logger ::log/entries
                     :uri-description ::database-definition))
(defn install-platform!
  "This installs the base-level datomic platform pieces"
  [logger uri-description]
  (let [details (schema/platform)]
    (install-schema! logger
                     uri-description
                     (-> details :partitions first)
                     details)))

(s/fdef install-schema-from-resource!
        :args (s/cat :this ::database-schema
                     :logger ::log/entries)
        :ret [any? ::log/entries])
(defn install-schema-from-resource!
  [{:keys [::db/uri
           ::db/partition-name
           ::db/protocol
           ::db/schema-resource-name]
    :as this} logs]
  (let [uri-description (::db/description uri)
        logs (log/debug logs
                        ::schema
                        "Installing"
                        {::description this
                         ::uri uri
                         ::protocol protocol
                         ::resource-file schema-resource-name})]
    (try
      (let [[tx-description logs] (load-transactions-from-resource! schema-resource-name logs)]
        (if tx-description
          (try
            (install-schema! logs
                             #_uri-description (::db/database-uri this)
                             partition-name
                             tx-description)
            (catch IllegalArgumentException ex
              (let [logs (log/exception logs
                                        ex
                                        ::installation
                                        "Schema failed"
                                        {::uri-description uri-description
                                         ::uri uri
                                         ::system this})]
                (throw (ex-info "Installation error"
                                {::logs logs})))))
          (throw (ex-info (str "No transactions in " schema-resource-name)
                          {::missing-transactions this
                           ::resource-name schema-resource-name
                           ::logs logs
                           ::keys (keys this)}))))
      (catch NullPointerException ex
        (throw (ex-info "Missing resource to load"
                        {::logs (log/exception logs
                                               ex
                                               ::load-resource
                                               "Not specified"
                                               this)}))))))
