(ns com.jimrthy.substratum.installer
  "Really just specs and wrappers around conformity and datomic-schema

It translates from a high-level description that makes sense to me into what they need

Note that the main point is to install the pieces defined in Antonio Andrade's
talk about datomic data platforms, as translated in the schema ns.

Actually, the main point is to use that data platform. This is another step
in that direction."
  (:require [clojure.spec :as s]
            [com.jimrthy.substratum.core]
            [datomic.api :as d]
            [datomic-schema.schema :refer [defdbfn
                                           fields]
             :as yuppie-schema]
            [com.jimrthy.substratum.core :as db]
            [com.jimrthy.substratum.schema :as schema]
            [com.jimrthy.substratum.util :as util]
            [io.rkn.conformity :as conformity]
            ;; TODO: Make this go away
            [taoensso.timbre :as log])
  (:import [clojure.lang ExceptionInfo]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specs

;; TODO: Be more restrictive
(s/def ::schema-resource-name string?)
(s/def ::uri :com.jimrthy.substratum.core/possible-uri-descriptions)
;; Q: Are there any restrictions on this?
(s/def ::partition-name string?)
;; A: Until there's something useful to do with start/stop,
;; just use a plain hashmap instead of a real defrecord Component.
;; It's tempting to make (start) convert the uri to a connection-string.
;; That temptation seems like a mistake.
(s/def ::database-schema (s/keys :req-un [::schema-resource-name
                                          :com.jimrthy.substratum.core/uri
                                          ::partition-name]))
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
(s/def ::transaction-sequence (s/coll-of ::individual-txn))

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
        :ret any?)
(defn do-schema-installation
  "Add schema/partition"
  [uri
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
        norms-map {partition-key {:txes [(vec transactions)]}}]
    (println "Conforming" conn "at" uri "\nto\n" norms-map "\nin" partition-name)
    ;; Q: Which of these do I want?
    (let [result (conformity/ensure-conforms conn norms-map)
          original (conformity/ensure-conforms conn norms-map [partition-key])]
      (log/debug "ensure-conforms returned:\n" result)
      ;; Returns nil on success
      result)))

(s/fdef load-transactions-from-resource
        :args (s/cat :resource-name string?)
        :ret ::txn-dscr-seq)
(defn load-transactions-from-resource
  [resource-name]
  (log/debug "Getting ready to load schema transaction from resource: "
             resource-name)
  (util/load-resource resource-name))

(defn schema-black-magic
  "Converts a pair of (attribute name, field descriptions) into something datomic-schema can expand.

Really just refactored out of a map description"
  [[attr field-descrs]]
  (log/debug "Individual attribute: " attr
             "\nDescription:\n" field-descrs)
  ;; Under the covers, Yuppiechef's schema macro just
  ;; calls schema*.
  (yuppie-schema/schema* (name attr)
                         {:fields (reduce (fn [acc [k v]]
                                            (comment (log/debug "Setting up field" k "with characteristics" v))
                                            (assoc acc (name k)
                                                   (if (= (count v) 1)
                                                     ;; If there isn't an option set,
                                                     ;; append one to a vector of the field.
                                                     ;; Because we might have a lazy seq here.
                                                     (do
                                                       (comment (log/debug "Adding default empty set"))
                                                       (conj (vec v) #{}))
                                                     (if (= (count v) 2)
                                                       v
                                                       (throw (ex-info (str "Bad field description\n"
                                                                            v
                                                                            " => "
                                                                            k) {:illegal-field-description v
                                                                           :field-id k}))))))
                                          {}
                                          field-descrs)}))

;;; Q: What does this return?
(s/fdef expand-schema-descr
        :args (s/cat :descr ::attr-txn-descr-seq)
        :ret any?)
(defn expand-schema-descr
  "Isolating a helper function to start expanding attribute descriptions into transactions"
  [descr]
  (log/info "Expanding Schema Description:\n"
            (util/pretty descr)
            "\na " (class descr))
  (map schema-black-magic descr))

(defn expanded-descr->schema
  "Take the output of expand-schema-descr (which should be identical
to the output of a seq of Yuppiechef's schema macro) and run it
through generate-schema to generate actual transactions"
  [attrs]
  (log/debug "expanded-descr->schema -- calling generate-schema on:\n"
             (util/pretty attrs) "\na " (class attrs))
  (yuppie-schema/generate-schema attrs {:index-all? true}))

(s/fdef expand-txn-descr
        :args (s/cat :descr ::txn-descr-seq)
        :ret ::transaction-sequence)
(defn expand-txn-descr
  "Convert from a slightly-more-readable high-level description
to the actual datastructure that datomic uses"
  [descr]
  (println "Expanding Transaction Description:\n"
           (util/pretty descr)
           "with keys:"
           (keys descr))
  (let [parts (map yuppie-schema/part (:partitions descr))
        attrs (expand-schema-descr (:attribute-types descr))
        generated-schema (expanded-descr->schema attrs)
        entities (:attributes descr)]
    {:structure (concat (yuppie-schema/generate-parts parts)
                        generated-schema)
     :data entities}))

;;; Q: Is there any point to this at all?
;;; A: Well...maybe it's worth specifying that other pieces
;;; need the schema installed in order to run?
;;; Seems like, honestly, it should just go away.
(s/fdef ctor
        :args (s/cat :config ::opt-database-schema)
        :ret ::database-schema)
(defn ^:deprecated ctor
  [config]
  ;; Because I'm just using a plain hashmap for now
  (select-keys config [:schema-resource-name :uri]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

;;; It's generally better to just install your schema from a resource
;;; definition.
;;; Recompiling code to modify database definitions went out of style
;;; back in the 70's.
;;; But doing it this way will probably always be more convenient from
;;; the REPL.
;;; And the main point behind this entire library is to set up the
;;; platform defined in the schema namespace so you (or, really,
;;; your customers) can use that as the foundation for the data
;;; they care about.

;;; Although there are really two pieces there:
;;; 1. Install the low-level platform schema
;;; 2. Install the higher-level data modelling schema for individual apps
;;; that's built on top of that platform

;; TODO: ^:always-validate
(s/fdef install-schema!
        :args (s/cat :uri-description :com.jimrthy.substratum.core/uri-description
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
        :ret any?)
(defn install-schema!
  [uri-description partition-name tx-description]
  (let [uri (db/build-connection-string uri-description)]
    (comment) (log/debug "Expanding high-level schema transaction description:\n"
                         (util/pretty tx-description))
        (let [{:keys [structure data]} (expand-txn-descr tx-description)]
          (comment) (log/debug "Setting up schema using\n"
                               (util/pretty structure) "at\n" uri)
          (try
            (if (s/valid? ::transaction-sequence structure)
              (do
                (doseq [step structure]
                  (do-schema-installation uri partition-name structure))

                ;; This has to happen as a Step 2:
                ;; We can't assign attributes to entities until
                ;; after the transaction that generates the schema
                (db/upsert! uri data))
              (throw (ex-info (str "Invalid transaction sequence:\n"
                                   (s/explain ::transaction-sequence structure)
                                   "Installing schema based on\n"
                                   (util/pretty structure)
                                   "\nwhich has" (count structure) "members")
                              {:problem-tx tx-description
                               :problem-struct structure
                               :tx-dscr tx-description
                               :uri uri
                               :uri-description uri-description})))))))

(defn install-platform!
  [uri-description]
  (let [details (schema/platform)]
    (install-schema! uri-description
                     (-> details :partitions first)
                     details)))

(s/fdef install-schema-from-resource!
        :args (s/cat :this ::database-schema)
        :ret any?)
(defn install-schema-from-resource!
  [this]
  (let [uri-description (-> this :uri :description)
        resource-name (:schema-resource-name this)
        partition-name (:partition-name this)]
    (comment (log/debug "Installing schema for\n" (util/pretty this)
                        "at" (util/pretty base-uri)
                        "using" (-> uri-description :protocol)
                        "\nfrom" resource-name))
    (if-let [tx-description (load-transactions-from-resource resource-name)]
      (install-schema! uri-description partition-name tx-description)
      (throw (ex-info (str "No transactions in " (:schema-resource-name this))
                      {:missing-transactions this
                       :resource-name (:schema-resource-name this)
                       :keys (keys this)})))))
