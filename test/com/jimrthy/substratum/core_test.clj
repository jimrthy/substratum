(ns com.jimrthy.substratum.core-test
  "Test core database functionality"
  (:require [clojure.test :refer (deftest is testing) :as test]
           [com.jimrthy.substratum.core :refer :all]
           ;; TODO: Convert this to component-dsl
           [com.stuartsierra.component :as component]))

(deftest memory-basics
  (let [uri-description {:db-name (str (gensym "core-memory-test-basics"))
                         :protocol :ram}
        disconnected (uri-ctor {:description uri-description})
        started (component/start disconnected)
        uri (:connection-string started)]
    (try
      ;; TODO: Have to install Schema first.
      ;; Which really means the appropriate Platform schemas so
      ;; I can use it to build meaningful Records.
      (upsert! uri [])
      (finally
        (component/stop started)))))
