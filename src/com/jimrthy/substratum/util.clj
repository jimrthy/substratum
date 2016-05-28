(ns com.jimrthy.substratum.util
  (:require [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [schema.core :as s])
  (:import [java.util Date UUID]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schema

(def async-channel (class (async/chan)))
(def java-byte-array (Class/forName "[B"))
;; FIXME: This should probably come from something like
;; simple-time instead
(def time-stamp Date)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

(s/defn ^:always-validate load-resource
  [url :- s/Str]
  (-> url
      clojure.java.io/resource
      slurp
      edn/read-string))

(defn now
  "According to SO, this is the idiomatic way to get the current time."
  []
  (java.util.Date.))

(defn log
  "Persist o for future reference.
  Really should do something more involved."
  [o]
  (throw (ex-info "Obsolete" {:todo "Switch to timbre instead"}))
  (let* [now (now)
         msg (format "%tH:%tM:%tS.%tL - %s%n" now now now now (str o))]
    (spit "event.log" msg :append true)
    msg))

;;; Do some brain-dead XML processing
(defn extract-from-node
  "This is brutal and inefficient...aside from being incorrect.
  Multiple instances of the same key will hide each other"
  [key node]
  (first (for [child node]
           (let [attributes (child :attrs)]
             (if (child key)
               (child key)
               (extract-from-node key (child :content)))))))

(defn extract-named-node
  "Note that this does no descent
That comment seems like a lie"
  [node]
  (loop [car (first node) cdr (rest node)]
    (when car
      (if (= :Named_Node (:tag car))
        (:attrs car)
        (recur (first cdr) (rest cdr))))))

(defn pretty
  [o]
  (with-out-str (pprint o)))

(s/defn random-uuid :- UUID
  []
  (UUID/randomUUID))

(s/defn walk-iterator
  "Like mapv over a java collection

Q: Is something like this handled in, say, flatland/useful?

TOOD: Handle multiple collections.
TODO: Make this lazy
TODO: Switch to using iterator-seq instead

In general, this shouldn't be needed.
But I'm running into a case specifically where
map is throwing
IllegalArgumentException Don't know how to create ISeq from: java.util.Collections$UnmodifiableCollection$1  clojure.lang.RT.seqFrom (RT.java:528)"
  [f
   it :- java.util.Collection]
  (loop [result []]
    (if (.hasNext it)
      (let [x (.next it)
            y (f x)]
        (recur (conj result y)))
      result)))
