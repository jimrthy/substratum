(ns com.jimrthy.substratum.core-test
  "Pretty much pointless, but it verifies that test harness works"
  (:require [clojure.test :refer (deftest is testing) :as test]
            [com.jimrthy.substratum.core :as substratum]))

(deftest connection-string-builder
  (let [db-name (str (gensym "core-memory-test-basics"))
        dscr {::substratum/db-name db-name
              ::substratum/protocol ::substratum/ram}
        cxn-string (substratum/build-connection-string dscr)]
    (is (= (str "datomic:mem://" db-name) cxn-string))))
(comment
  (substratum/build-connection-string {:com.jimrthy.substratum.core/protocol ::substratum/free})
  )

(deftest memory-basics
  (let [uri-description {::substratum/db-name (str (gensym "core-memory-test-basics"))
                         ::substratum/protocol ::substratum/ram}
        logger []
        [started logger] (substratum/start! logger uri-description)
        uri (::substratum/connection-string started)]
    ;; TODO: Have to install Schema first.
    ;; Which really means the appropriate Platform schemas so
    ;; I can use it to build meaningful Records.
    (substratum/upsert! uri [])))
