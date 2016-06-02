(ns com.jimrthy.substratum.platform
  "Based upon the 'Datomic as a Data Platform' talk by Antonio Andrade"
  (:require [com.stuartsierra.component :as component]
            [datomic.api :as d]
            [datomic-schema.schema :refer [defdbfn
                                           fields
                                           generate-parts
                                           generate-schema
                                           part
                                           schema*]]
            [com.jimrthy.substratum.core :as db]
            [com.jimrthy.substratum.util :as util]
            [hara.event :refer [raise]]
            [io.rkn.conformity :as conformity]
            [schema.core :as s]
            [taoensso.timbre :as log])
  (:import [clojure.lang ExceptionInfo]
           [com.jimrthy.substratum.core URL]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Prismatic Schema

;; Q: Is there anything to do w/ startup/shutdown?
(comment (s/defrecord DatabaseSchema [schema-resource-name :- s/Str
                                      uri :- fig.db.core.URL]
           component/Lifecycle
           (start
            [this]
            (raise :not-implemented))
           (stop
            [this]
            (raise :not-implemented))))
;; A: Until there is, just use a plain hashmap
(defn DatabaseSchema []
  {:schema-resource-name s/Str
   :uri URL})

(defn uniqueness []
  (s/enum :db.unique/identity  ; attempts to insert dupe value for different entity will fail
          :db.unique/value))   ; attempts to insert dupe value for entity w/ tempid will merge existing entity

(defn value-types []
  (s/enum :db.type/bigdec
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
          :db.type/uuid))

(defn cardinality-options []
  (s/enum :db.cardinality/one
          :db.cardinality/many))

(defn SchemaTransaction []
  (into (db/BaseTransaction) {:db/ident s/Keyword
                              :db/cardinality (cardinality-options)
                              :db/valueType (value-types)
                              ;; TODO: We could also do alterations
                              :db.install/_attribute s/Keyword ; must be :db.part/db
                              (s/optional-key :db/doc) s/Str
                              (s/optional-key :db/fulltext) s/Bool ; Generate an eventually consistent fulltext search
                              (s/optional-key :db/index) s/Bool
                              (s/optional-key :db/isComponent) s/Bool ; ref attributes become sub-components
                              (s/optional-key :db/no-history) s/Bool
                              (s/optional-key :db.unique) (uniqueness)}))

;; This could be done as two steps in one transaction...but why?
(defn PartitionTransaction []
  (into (db/BaseTransaction) {:db/ident s/Keyword
                              ;; Must be :db.part/db
                              :db.install/_partition s/Keyword}))

(defn IndividualTxn []
  (s/either (SchemaTransaction) (PartitionTransaction) (db/UpsertTransaction) (db/RetractTxn)))
(defn TransactionSequence
  []
  [(IndividualTxn)])

(defn PartTxnDescrSeq
  "Really just a sequence of names"
  []
  [s/Str])

(defn AttrTxnDescr []
  {s/Symbol
   [ ;; These are almost value-types,
    ;; but YuppieChef adds the namespace for us
    (s/one s/Keyword "primitive-type")
    (s/optional #{(s/either s/Str s/Keyword)} "options")]})

(defn AttrTxnDescrSeq []
  {s/Symbol (AttrTxnDescr)})

(defn TxnDescrSeq []
  [(s/one (PartTxnDescrSeq) "parts")
   (s/one (AttrTxnDescrSeq) "attributes")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal

(s/defn ^:always-validate obsolete-transact-schema! :- db/TransactionResult
  [conn :- datomic.peer.LocalConnection
   txns :- [(s/either (SchemaTransaction) (PartitionTransaction))]]
  (raise {:obsolete "Use do-schema-installation instead"})
  @(d/transact conn txns))

(s/defn ^:always-validate do-schema-installation
  "Add schema/partition"
  [uri :- s/Str
   partition-name :- s/Str
   transactions :- (TransactionSequence)]
  (d/create-database uri)
  (let [conn (d/connect uri)
        partition-key (keyword partition-name "base-schema")
        ;; TODO: Generate this part using either schematode
        ;; or yuppiechef's library instead
        ;; Or maybe break down and set up my own data platform
        norms-map {partition-key {:txes [transactions]}}]
    ;; Returns nil on success
    (throw (ex-info "Start here" {:problem "No transactions provided for norm"}))
    (conformity/ensure-conforms conn norms-map [partition-name])))

(s/defn load-transactions-from-resource :- (TxnDescrSeq)
  [resource-name :- s/Str]
  (println "Loading transactions from resource: '" resource-name "'")
  (util/load-resource resource-name))

(s/defn expand-schema-descr
  "Isolating a helper function to start expanding attribute descriptions into transactions"
  [descr :- (AttrTxnDescrSeq)]
  (map (fn [[attr field-descrs]]
         ;; I'm duplicating some of the functionality from
         ;; Yuppiechef's library because he has it hidden
         ;; behind macros
         (schema* (name attr)
                  {:fields (reduce (fn [acc [k v]]
                                     (comment (log/debug "Setting up field" k "with characteristics" v))
                                     (assoc acc (name k)
                                            (if (= (count v) 1)
                                              ;; If there isn't an option set,
                                              ;; use vec to make sure one gets appended
                                              (do
                                                (comment (log/debug "Adding default empty set"))
                                                (conj (vec v) #{}))
                                              (if (= (count v) 2)
                                                v
                                                (raise {:illegal-field-description v
                                                        :field-id k})))))
                                   {}
                                   field-descrs)}))
       descr))

(defn expanded-descr->schema
  "Take the output of expand-schema-descr (which should be identical
to the output of a seq of Yuppiechef's schema macro) and run it
through generate-schema to generate actual transactions"
  [attrs]
  (comment (map (fn [namespace]
                  (generate-schema namespace {:index-all? true}))
                attrs))
  (generate-schema attrs {:index-all? true}))

(s/defn expand-txn-descr :- (TransactionSequence)
  "Convert from a slightly-more-readable high-level description
to the actual datastructure that datomic uses"
  [descr :- TxnDescrSeq]
  (let [parts (map part (first descr))
        attrs (expand-schema-descr (second descr))
        generated-schema (expanded-descr->schema attrs)
        entities (nth descr 2)]
    [(concat (generate-parts parts)
             generated-schema)
     entities]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

(s/defn install-schema!
  [this :- (DatabaseSchema)
   partition-name]
  (let [base-uri (:uri this)
        resource-name (:schema-resource-name this)]
    (comment) (log/debug "Installing schema for\n" (util/pretty this)
                         "at" (util/pretty base-uri)
                         "using" (-> base-uri :description :protocol)
                         "\nfrom" resource-name)
    (if-let [tx-description (load-transactions-from-resource resource-name)]
      (let [uri (-> this :uri :description db/build-connection-string)]
        (comment) (log/debug "Expanding high-level schema transaction description:\n"
                             (util/pretty tx-description)
                             "from" resource-name)
        (let [[schema-tx primer-tx] (expand-txn-descr tx-description)]
          (comment) (log/debug "Setting up schema using\n"
                               (util/pretty schema-tx) "at\n" uri
                               "\nand priming with\n"
                               (util/pretty primer-tx))
          (try
            (s/validate (TransactionSequence) schema-tx)
            (catch ExceptionInfo ex
              (log/error ex "Installing schema based on\n"
                         #_(util/pretty tx) schema-tx
                         "\nwhich has" (count schema-tx) "members"
                         "is going to fail")
              (doseq [step schema-tx]
                (try
                  (s/validate IndividualTxn step)
                  (catch ExceptionInfo ex
                    (log/error ex "Step:\n" step)))))
            (catch Exception ex
              (log/error ex "Low level schema installation error based on\n"
                         schema-tx
                         "\nwhich has" (count schema-tx) "members")))
          (do-schema-installation uri partition-name schema-tx)

          ;; This has to happen as a Step 2:
          ;; We can't assign attributes to entities until
          ;; after the transaction that generates the schema
          (db/upsert! uri primer-tx)))
      (raise {:missing-transactions this
              :resource-name (:schema-resource-name this)
              :keys (keys this)}))))

(s/defn ctor :- (DatabaseSchema)
  [config]
  ;; Because I'm just using a plain hashmap for now
  (comment (map->DatabaseSchema config))
  (select-keys config [:schema-resource-name :uri]))
