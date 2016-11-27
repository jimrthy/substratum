(defproject com.jimrthy.substratum "0.1.0-SNAPSHOT"
  :dependencies [[com.datomic/datomic-free "0.9.5394" :exclusions [org.clojure/tools.cli]]
                 ;; Since this is supposed to be a library,
                 ;; I really shouldn't even depend on either
                 ;; of these
                 [com.jimrthy/component-dsl "0.1.2-SNAPSHOT"  :exclusions [com.google.guava/guava]]
                 [com.stuartsierra/component "0.3.1"]
                 ;; Q: How much value-add do I get here?
                 ;; A: Not enough.
                 ;; If end-users want to do attribute schema
                 ;; short-hand, I shouldn't force one on them
                 [datomic-schema "1.3.0"]
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
                 [org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/core.async "0.2.391"]
                 [org.clojure/tools.cli "0.3.1"]]
  :description "Data Platform on Datomic"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {:dev {:resource-paths ["dev-resources"]
                   :source-paths ["dev"]}}
  :url "https://github.com/jimrthy/substratum")
