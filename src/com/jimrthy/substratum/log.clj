(ns com.jimrthy.substratum.log
  "Accumulate batches of logs. Do the side-effects later in isolation"
  (:require [clojure.spec.alpha :as s])
  (:import java.io.OutputStream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specs

(def log-levels #{::trace
                 ::debug
                 ::info
                 ::warn
                 ::error
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

(deflogger trace)
(deflogger debug)
(deflogger info)
(deflogger warn)
(deflogger error)
(deflogger fatal)

;;; Implement this for your side-effects
(defprotocol Logger
  (log [this msg]))

(defrecord StreamLogger [stream]
  Logger
  (log [{^OutputStream stream :stream
         :as this}
        msg]
    (.write stream (pr-str msg))))

(s/fdef flush-logs
        :args (s/cat :logger #(satisfies? Logger %)
                     :logs ::entries))
(defn flush-logs
  "For the side-effects to write the accumulated logs"
  [logger
   log-collection]
  (doseq [message log-collection]
    (log logger message)))
