(ns com.jimrthy.substratum.platform-test
  "Unit testing the database is generally considered a bad idea, but I have to start somewhere"
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer [are deftest is testing use-fixtures]]
            [com.jimrthy.substratum.core :as db]
            [com.jimrthy.substratum.platform :as platform]
            [com.jimrthy.substratum.util :as util]
            [com.stuartsierra.component :as component]
            [component-dsl.system :as cpt-dsl]
            [datomic.api :as d]
            [datomic-schema.schema :refer [defdbfn
                                           fields
                                           generate-parts
                                           generate-schema
                                           part
                                           schema]]
            [io.rkn.conformity :as conformity]
            [hara.event :refer [raise]]
            [schema.core :as s]
            [taoensso.timbre :as log])
  (:import [clojure.lang ExceptionInfo IExceptionInfo]
           [datomic.impl Exceptions$IllegalArgumentExceptionInfo]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Globals
;;; Because it doesn't seem worth the time to figure out how
;;; to do this correctly
;;; (the problem is that tests don't accept any parameters,
;;; but I really need to pass a new system to each)
(def system nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Boilerplate

(defn system-for-testing
  []
  (let [base-system (cpt-dsl/ctor "admin.test-system.edn")]
    (assoc base-system :database-uri (db/uri-ctor {:description {:name (gensym)
                                                                 :protocol :ram}}))))

(defn in-mem-db-system
  "The really annoying thing about this approach is that I
can't just call the individual tests manually"
  [f]
  (try
    (let [pre-testable-system (system-for-testing)]
      ;; Most tests probably make sense to run against that
      ;; TODO: Add test features
      (try
        (let [started-system (component/start pre-testable-system)]
          (try
            (alter-var-root #'system (constantly started-system))
            (f)
            (catch RuntimeException ex
              (log/error ex "FAIL"))
            (finally
              ;; Better safe than sorry
              (let [stopped (component/stop started-system)]
                (alter-var-root #'system (constantly stopped))))))
        (catch RuntimeException ex
          (log/error ex "Setting up baseline admin system"))))))

(use-fixtures :each in-mem-db-system)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

(s/defn ^:always-check extract-connection-string :- s/Str
  []
  (-> system :database-uri :connection-string))

(defn test-partition
  []
  [(part "test")])

(defn base-datatype-schema-txn
  "In reality, this should be loaded from EDN"
  []
  [(schema dt
           (fields
            [dt :ref "Think of an object's class"]
            [namespace :string "Think of a class' package"]
            [name :string "Think of a class' simple name"]
            [parent :ref "Superclass"]
            [list :ref "For types that are a sequence of something"]
            [component :ref "The Datatype of the instances that go into a list"]
            [fields :ref :many "Think of a class' member variables"]))])

(defn base-datatype-txn
  "Transaction for generating the base datatype entity"
  []
  [{:db/id :dt/dt
    :dt/dt :dt/dt
    :dt/namespace "system"
    :dt/name "Datatype"
    :dt/fields [:dt/dt :dt/namespace :dt/name :dt/parent
                :dt/list :dt/component :dt/fields]}])

(defn base-part-and-data-type-txn
  "For setting up minimalist baseline schema"
  []
  (let [parts (test-partition)
        schema (base-datatype-schema-txn)]
    (concat
     (generate-parts parts)
     (generate-schema schema {:index-all? true}))))

(defn base-datatype-query
  []
  '{:find [?e]
    :where [[?e :dt/dt :dt/dt]]})

(comment (let [lhs {:db/doc "N-d sequences",
                    :db/index true,
                    :db.install/_attribute :db.part/db,
                    :db/id #datomic.db.DbId{:part :db.part/db, :idx -1000126},
                    :db/ident :dt/list,
                    :db/valueType :db.type/ref,
                    :db/cardinality :db.cardinality/one}
               rhs {:db/doc "N-d sequences",
                    :db/index true,
                    :db.install/_attribute :db.part/db,
                    :db/id #datomic.db.DbId{:part :db.part/db, :idx -1000136},
                    :db/ident :dt/list,
                    :db/valueType :db.type/ref,
                    :db/cardinality :db.cardinality/one}]
           (if (= (dissoc lhs :db/id) (dissoc rhs :db/id))
             (println "They're equal")
             (println "What's the difference?"))))

(defn verify-same-elements
  "Verify that each element in sequence 1 has exactly 1 counterpart in sequence 2,
and vice versa"
  [s1 s2]
  ;; First, check for duplicates
  (let [set1 (set s1)]
    (when (not= (count set1) (count s1))
      (raise {:duplicate-entry s1})))
  (let [set2 (set s2)]
    (when (not= (count set2) (count s2))
      (raise {:duplicate-entry s2})))

  ;; Then check for exclusions
  ;; This approach isn't valid.
  ;; The tempid should definitely not be the same
  (comment (doseq [lhs s1]
             (when-not (some #(= lhs %) s2)
               (raise {:rhs-missing lhs})))
           (doseq [rhs s2]
             (when-not (some #(= rhs %) s1)
               (raise {:lhs-missing rhs}))))
  (doseq [lhs s1]
    (let [matches
          (filter identity (map (fn [rhs]
                                  (when (= (dissoc lhs :db/id)
                                           (dissoc rhs :db/id))
                                    true))
                                s2))]
      (when (not= 1 (count matches))
        (raise {:wrong-left-hand-match lhs
                :matches matches}))))
  (doseq [rhs s2]
    (let [matches
          (filter identity (map (fn [lhs]
                                  (when (= (dissoc lhs :db/id)
                                           (dissoc rhs :db/id))
                                    true))
                                s1))]
      (when (not= 1 (count matches))
        (raise {:wrong-right-hand-match rhs
                :matches matches})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Actual Tests

(deftest basic-structure
  "Because I have to start somewhere...does setup/teardown work?"
  (is (= 2 2)))

(deftest note-syntax
  "Because I haven't played w/ are before"
  (are [x y] (= 5 (+ x y))
       2 3
       1 4
       6 -1))

(deftest datatype-schema
  "Prove that I can I can add minimalist schema to the database
Yes, this is almost copy/pasted from install-schema!

And it's probably a prime example of why unit testing your database
is a bad idea.

But, seriously. I had to start somewhere."
  (let [cxn-str (extract-connection-string)
        ;; TODO: Don't do this here
        ;; Q: Well, then, where?
        structural-txn (base-part-and-data-type-txn)
        sql (base-datatype-query)
        initial-txn (base-datatype-txn)]
    (is (not (conformity/has-attribute? (-> cxn-str d/connect d/db) :dt/dt)))
    (try
      (db/q sql cxn-str)
      ;; That should throw...something
      (is false)
      (catch Exception ex
        ;; This is the expected course of events.
        ;; Although the fact that they're throwing a raw Exception is
        ;; pretty annoying
        (let [cause (.getCause ex)
              root-cause (.getCause cause)
              base-details {:instance cause
                            :class (class cause)
                            :message (.getMessage cause)
                            :cause root-cause}
              details {:instance ex
                       :class (class ex)
                       :message (.getMessage ex)
                       :cause cause}
              cause-of-root (.getCause root-cause)  ; yes, this is ugly
              root-details {:instance root-cause
                            :class (class root-cause)
                            :message (.getMessage root-cause)
                            :cause root-cause}]
          ;; Verify that ex matches the "no such attribute" error
          (is (nil? cause-of-root))
          (is (= (:class root-details) Exceptions$IllegalArgumentExceptionInfo))
          (is (= ":db.error/not-an-entity Unable to resolve entity: :dt/dt" (:message root-details))))
        (let [migration-ret-val
              (platform/do-schema-installation cxn-str "silly-test" structural-txn)]
          (is (not migration-ret-val)))
        (is (conformity/has-attribute? (-> cxn-str d/connect d/db) :dt/dt))
        (let [datatypes (db/q sql cxn-str)]
          (is (= #{} datatypes)))
        (let [;; This does the deref for us
              insertion (db/upsert! cxn-str initial-txn)]
          (let [datatypes (db/q sql cxn-str)]
            (is (= 1 (count datatypes))))))
      (catch Throwable ex
        (raise {:unexpected ex
                :connection-string cxn-str
                :query sql
                :system system})))))

(deftest check-edn-install
  "Make sure the schema.edn does what I expect"
  []
  (let [cxn-str (extract-connection-string)
        ;; Really shouldn't be caching this,
        ;; at least in theory. But it seems
        ;; silly not to, in practice.
        ;; It's not like I'm actively passing it around
        ;; anywhere else
        conn (d/connect cxn-str)]
    (is (not (conformity/has-attribute? (d/db conn) :dt/dt)))
    (let [schema-cpt (:database-schema system)]
      (platform/install-schema! "edn-test" schema-cpt))
    (is (conformity/has-attribute? (d/db conn) :dt/dt))))

(deftest data-platform-basics
  []
  (let [cxn-str (extract-connection-string)
        conn (d/connect cxn-str)]
    (let [schema-cpt (:database-schema system)]
      (platform/install-schema! schema-cpt "testing-platform-basics"))
    ;; OK, we should have everything set up to let
    ;; us start rocking and rolling with our kick-ass
    ;; Data Platform.
    ;; Q: What next?
    (is false "Do something useful with this")))

;;; This is what the attribute-expansion test below amounts to
(comment (let [baseline [(schema dt (fields [dt :ref "Think of an object's class"]
                                            [namespace :string "package"]
                                            [name :string "getSimpleName"]
                                            [parent :ref "super"]
                                            [list :ref "N-d sequences"]
                                            [component :ref "Type that list contains"]))
                         (schema dt.any (fields [bigdec :bigdec]
                                                [bigint :bigint]
                                                [boolean :boolean]))]
               canonical (generate-schema baseline
                                          {:index-all? true})
               my-description '{dt {dt [:ref #{"Think of an object's class"}]
                                    namespace [:string #{"package"}]
                                    name [:string #{"getSimpleName"}]
                                    parent [:ref #{"super"}]
                                    list [:ref #{"N-d sequences"}]
                                    component [:ref #{"Type that list contains"}]}
                                dt.any {bigdec [:bigdec]
                                        bigint [:bigint]
                                        boolean [:boolean]}}
               my-expansion (platform/expand-schema-descr my-description)
               my-generation (platform/expanded-descr->schema my-expansion)]
           (println "Base Description")
           (pprint baseline)
           (println "My Version")
           (pprint my-expansion)
           (if (= baseline my-expansion)
             (println "So far, so good")
             (println "There really isn't any hope"))
           (println "Real Transaction")
           (pprint canonical)
           (println "What I generated")
           (pprint my-generation)
           ;; This is too simplistic:
           ;; Since they're sequences that were generated from maps,
           ;; the order isn't deterministic
           (if (= canonical my-generation)
             (println "Success!!")
             (println "Boo"))
           (try
             (verify-same-elements canonical my-generation)
             (println "But it's OK in the end")
             (catch clojure.lang.ExceptionInfo ex
               (println "It's really a failure")
               (println (.getMessage ex))
               (println (ex-data ex))))))

(deftest attribute-expansion
  "Yuppiechef uses macros.
Translating them into functions that work on EDN
hasn't been trivial"
  []
  (let [baseline [(schema dt (fields [dt :ref "Think of an object's class"]
                                     [namespace :string "package"]
                                     [name :string "getSimpleName"]
                                     [parent :ref "super"]
                                     [list :ref "N-d sequences"]
                                     [component :ref "Type that list contains"]))
                  (schema dt.any (fields [bigdec :bigdec]
                                         [bigint :bigint]
                                         [boolean :boolean]))]
        canonical (generate-schema baseline
                                   {:index-all? true})
        my-description '{dt {dt [:ref #{"Think of an object's class"}]
                             namespace [:string #{"package"}]
                             name [:string #{"getSimpleName"}]
                             parent [:ref #{"super"}]
                             list [:ref #{"N-d sequences"}]
                             component [:ref #{"Type that list contains"}]}
                         dt.any {bigdec [:bigdec]
                                 bigint [:bigint]
                                 boolean [:boolean]}}
        my-expansion (platform/expand-schema-descr my-description)
        my-generation (platform/expanded-descr->schema my-expansion)]
    (is (= baseline my-expansion))
    (if (not= canonical my-generation)
      (verify-same-elements canonical my-generation)
      (is false "They can't be = due to tempids"))))

;; I feel like I should be putting things into something like
;; this, but this is really for sub-contexts inside a deftest
(comment (testing "Does the schema manipulation make sense?"))
