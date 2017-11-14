(ns com.jimrthy.substratum.log
  "Accumulate batches of logs. Do the side-effects later in isolation"
  (:require [clojure.spec.alpha :as s])
  (:import clojure.lang.ExceptionInfo
           java.io.OutputStream))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specs

(def log-levels #{::trace
                  ::debug
                  ::info
                  ::warn
                  ::error
                  ::exception
                  ::fatal})
(s/def ::level log-levels)

(s/def ::label keyword?)

;; Go with milliseconds since epoch
(s/def ::time nat-int?)

(s/def ::message string?)

(s/def ::details any?)

(s/def ::entry (s/keys :req [::level
                             ::label
                             ::time
                             ::message]
                       :opt [::details]))

(s/def ::entries (s/coll-of ::entry))

;;; Implement this for your side-effects
(defprotocol Logger
  (log! [this msg])
  (flush! [this]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal

(s/fdef log :args (s/cat :entries ::entries
                         :level ::level
                         :message ::message
                         :details ::details))
(defn add-log-entry
  ([entries
    level
    label
    message
    details]
   (conj entries {::level level
                  ::label label
                  ::time (System/currentTimeMillis)
                  ::message message
                  ::details details}))
  ([entries
    level
    label
    message]
   (conj entries {::level level
                  ::label label
                  ::time (System/currentTimeMillis)
                  ::message message})))

(defmacro deflogger
  [level]
  ;; I'd much rather do something like this for the sake of hygiene:
  (comment
    `(let [lvl-holder# '~level
           tag-holder# (keyword (str *ns*) (name lvl-holder#))]
       (defn '~lvl-holder#
         ([entries#
           label#
           message#
           details#]
          (add-log-entry entries# ~'~tag-holder label# message# details#))
         ([entries#
           label#
           message#]
          (add-log-entry entries# ~'~tag-holder label# message#)))))
  (let [tag (keyword (str *ns*) (name level))]
    ;; The auto-gensymmed parameter names are obnoxious
    `(defn ~level
       ([entries#
         label#
         message#
         details#]
        (add-log-entry entries# ~tag label# message# details#))
       ([entries#
         label#
         message#]
        (add-log-entry entries# ~tag label# message#)))))

(defrecord StreamLogger [stream]
  ;; I think this is mostly correct,
  ;; but I haven't actually tried testing it
  Logger
  (log! [{^OutputStream stream :stream
         :as this}
        msg]
    (.write stream (pr-str msg)))
  (flush! [{^OutputStream stream :stream
            :as this}]
    (.flush stream)))

(defrecord StdOutLogger []
  ;; Really just a StreamLogger
  ;; where stream is STDOUT.
  ;; But it's simple/easy enough that it seemed
  ;; worth writing this way instead
  Logger
  (log! [_ msg]
    (println (pr-str msg)))
  (flush! [_]
    ;; Q: Is there any point to calling .flush
    ;; on STDOUT?
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

(deflogger trace)
(deflogger debug)
(deflogger info)
(deflogger warn)
(deflogger error)

(defn exception
  ([entries ex label message]
   (exception entries ex label message nil))
  ([entries ex label message original-details]
   (let [details {::original-details original-details
                  ::stack (.getStackTrace ex)
                  ::exception ex}
         details (if (instance? ExceptionInfo ex)
                   (assoc details ::data (.getData ex))
                   details)]
     (add-log-entry entries ::exception label message details))))

(deflogger fatal)

(s/fdef flush-logs!
        :args (s/cat :logger #(satisfies? Logger %)
                     :logs ::entries))
(defn flush-logs!
  "For the side-effects to write the accumulated logs"
  [logger
   log-collection]
  (doseq [message log-collection]
    (log! logger message))
  (flush! logger))
