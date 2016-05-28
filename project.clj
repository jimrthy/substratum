(defproject com.jimrthy.substratum "0.1.0-SNAPSHOT"
  :dependencies [[com.datomic/datomic-free "0.9.5359"]
                 ;; Since this is supposed to be a library,
                 ;; I really shouldn't even depend on either
                 ;; of these
                 [com.jimrthy/component-dsl "0.1.1-SNAPSHOT"]
                 [com.stuartsierra/component "0.3.1"]
                 ;; This really doesn't seem worth including
                 [com.taoensso/timbre "4.3.1"]
                 [datomic-schema "1.3.0"]
                 ;; This seems even worse
                 [im.chit/hara.event "2.2.17"]
                 ;; This seems very debatable
                 ;; End-users probably need to be responsible
                 ;; for their own data migration paths
                 ;; Except that that's really where this entire
                 ;; ball of mud turns interesting.
                 ;; Hmm...it's much easier to add features than
                 ;; take them away.
                 ;; Then again, I'm only trying to refactor this out
                 ;; of the library where it currently lives tonight.
                 ;; There should be plenty of time to make it useful
                 ;; for others after it's useful for me.
                 ;; So just port what I've already written.
                 [io.rkn/conformity "0.4.0"]
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.374"]
                 ;; I even have my doubts about this
                 [prismatic/schema "1.0.5"]
                 ]
  :description "Data Platform on Datomic"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :url "https://github.com/jimrthy/substratum")
