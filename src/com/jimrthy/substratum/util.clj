(ns com.jimrthy.substratum.util
  (:require [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.pprint :refer (pprint)]
            [clojure.spec :as s])
  (:import [java.util Iterator Date UUID]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specs

(def async-channel-type (class (async/chan)))
(s/def ::async-channel #(instance? async-channel-type %))
(def java-byte-array (Class/forName "[B"))
(s/def ::java-byte-array #(instance? java-byte-array %))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

(s/fdef load-resource
        ;; Q: Can I get any more specific than this?
        :args (s/cat :url string?)
        :ret any?)
(defn load-resource
  [url]
  (-> url
      clojure.java.io/resource
      slurp
      edn/read-string))

(s/fdef now
        :args (s/cat)
        :ret inst?)
(defn now
  "According to SO, this is the idiomatic way to get the current time."
  []
  (Date.))

(defn log!
  "Persist o for future reference.
  Really should do something more involved.
  Then again...we have commons-logging, log4j, and jboss.logging all available.
  Which means this should probably just go away."
  ([file-name o]
   (let* [now (now)
          msg (format "%tH:%tM:%tS.%tL - %s%n" now now now now (str o))]
     (spit file-name msg :append true)
     msg))
  ([o]
   (log! "substratum.log" o)))

(defn extract-from-node
  "Find the first (depth-first) child of node that matches key.

In some way that might or might not involve the attributes.

This needs some serious attention/deletion, but that needs to happen
some other day.

TODO: Sort this out and hopefully just make it go away."
  [key node]
  (first (for [child node]
           (let [attributes (:attrs child)]
             (or (child key)
                 (extract-from-node key (:content child)))))))

(defn extract-named-node
  "This is another function that needs some thorough consideration.

Note that this does no descent
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

(s/fdef random-uuid
        :args (s/cat)
        :ret uuid?)
(defn random-uuid
  []
  (UUID/randomUUID))

(s/fdef walk-iterator
        :args (s/cat :f (s/fspec :args (s/cat :x any?)
                                 :ret any?)
                     ;; There really isn't any point unless this is a java.util.Collection
                     :it (s/coll-of any?))
        ;; TODO: Add a :fn spec.
        ;; The spec for :it matches :f's :x spec.
        ;; (= (-> % :args :f :ret ::spec)
        ;;    (-> % :ret contained-spec))
        ;; and
        #_ (= (-> % :args :it count)
              (-> % :ret count))
        ;; although that latter screws up any hope of laziness
        :ret (s/coll-of any?))
(defn walk-iterator
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
   ^Iterator it]
  (loop [result []]
    (if (.hasNext it)
      (let [x (.next it)
            y (f x)]
        (recur (conj result y)))
      result)))
