(ns com.jimrthy.substratum.schema
  "Schema associated with Antonio Andrade's Datomic Data Platform.
  This is really the heart of substratum."
  (:require [datomic.api :as d]))

(defn platform
  []
  {:attribute-types '{;; Baseline attribute type definitions
                      dt {dt [:ref #{"Think of an object's class"}]
                          namespace [:string #{"Think of a class' package"}]
                          name [:string #{".getSimpleName"}]
                          parent [:ref #{"For Type hierarchies and inheritance: .getSuperClass"}]
                          list [:ref #{"For N-d lists"}]
                          component [:ref #{"If this Type is a list, this describes what that list contains"}]
                          fields [:ref #{"Data Type references to more Data Types" :many}]
                          any [:ref #{"Built-in types enumeration. Needs to point to a dt.any Entity"}]}
                      ;; These next are for the Any/Variant type
                      dt.any {bigdec [:bigdec]
                              bigint [:bigint]
                              boolean [:boolean]
                              bytes [:bytes]
                              double [:double]
                              float [:float]
                              instant [:instant]
                              keyword [:keyword]
                              long [:long]
                              ref [:ref]
                              string [:string]
                              uri [:uri]
                              uuid [:uuid]}}
   ;; Q: Didn't these actually get defined in the :attribute-types section above?
   ;; A: No. Those define the attribute schemas.
   ;; This defines attributes that use those schemas.
   :attributes [;; :dt/dt is both the definition of a data type and an instance of itself
                {:db/id :dt/dt
                 :dt/dt :dt/dt
                 :dt/namespace "system"
                 :dt/name "Datatype"
                 :dt/fields [:dt/dt :dt/namespace :dt/name :dt/parent
                             :dt/list :dt/component :dt/fields]}
                ;; Any/Variant Type
                ;; Define a type, then enumeration values
                {:db/id #db/id[:db.part/db]
                 :db/ident :dt/any
                 :db/valueType :db.type/ref
                 :db/cardinality :db.cardinality/one
                 :db/doc "Built-in types enumeration"
                 :db/install/_attriute :db.part/db}
                {:db/id #db/id[:db.part/db]
                 :db/ident :dt.any/bigdec
                 :db/valueType :db.type/bigdec
                 :db/cardinality :db.cardinality/one
                 :db.install/_attribute :db.part/db}
                {:db/id #db/id[:db.part/db]
                 :db/ident :dt.any/bigint
                 :db/valueType :db.type/bigint
                 :db/cardinality :db.cardinality/one
                 :db.install/_attribute :db.part/db}
                {:db/id #db/id[:db.part/db]
                 :db/ident :dt.any/boolean
                 :db/valueType :db.type/boolean
                 :db/cardinality :db.cardinality/one
                 :db.install/_attribute :db.part/db}
                {:db/id #db/id[:db.part/db]
                 :db/ident :dt.any/bytes
                 :db/valueType :db.type/bytes
                 :db/cardinality :db.cardinality/one
                 :db.install/_attribute :db.part/db}
                {:db/id #db/id[:db.part/db]
                 :db/ident :dt.any/double
                 :db/valueType :db.type/double
                 :db/cardinality :db.cardinality/one
                 :db.install/_attribute :db.part/db}
                {:db/id #db/id[:db.part/db]
                 :db/ident :dt.any/float
                 :db/valueType :db.type/float
                 :db/cardinality :db.cardinality/one
                 :db.install/_attribute :db.part/db}
                {:db/id #db/id[:db.part/db]
                 :db/ident :dt.any/instant
                 :db/valueType :db.type/instant
                 :db/cardinality :db.cardinality/one
                 :db.install/_attribute :db.part/db}
                {:db/id #db/id[:db.part/db]
                 :db/ident :dt.any/keyword
                 :db/valueType :db.type/keyword
                 :db/cardinality :db.cardinality/one
                 :db.install/_attribute :db.part/db}
                {:db/id #db/id[:db.part/db]
                 :db/ident :dt.any/long
                 :db/valueType :db.type/long
                 :db/cardinality :db.cardinality/one
                 :db.install/_attribute :db.part/db}
                {:db/id #db/id[:db.part/db]
                 :db/ident :dt.any/ref
                 :db/valueType :db.type/ref
                 :db/cardinality :db.cardinality/one
                 :db.install/_attribute :db.part/db}
                {:db/id #db/id[:db.part/db]
                 :db/ident :dt.any/string
                 :db/valueType :db.type/string
                 :db/cardinality :db.cardinality/one
                 :db.install/_attribute :db.part/db}
                {:db/id #db/id[:db.part/db]
                 :db/ident :dt.any/uri
                 :db/valueType :db.type/uri
                 :db/cardinality :db.cardinality/one
                 :db.install/_attribute :db.part/db}
                {:db/id #db/id[:db.part/db]
                 :db/ident :dt.any/uuid
                 :db/valueType :db.type/uuid
                 :db/cardinality :db.cardinality/one
                 :db.install/_attribute :db.part/db}

                ;; And then the enumeration that brings those all together
                {:db/id :dt/any
                 :dt/dt :dt/dt
                 :dt/namespace "system"
                 :dt/name "Any"
                 :dt/fields [:dt/dt
                             :dt/any
                             ;; Only one of these is present at runtime
                             ;; for any given "Any" entity
                             :dt.any/bigdec
                             :dt.any/bigint
                             :dt.any/boolean
                             :dt.any/bytes
                             :dt.any/double
                             :dt.any/float
                             :dt.any/instant
                             :dt.any/keyword
                             :dt.any/long
                             :dt.any/ref
                             :dt.any/string
                             :dt.any/uri
                             :dt.any/uuid]}
                ;; TODO: Still have to cope w/ Typed References
                ;; And then things start getting interesting w/ migrations
                ]
   ;; Q: Is there a good reason for this being set up as multiples?
   :partitions ["substratum"]})
