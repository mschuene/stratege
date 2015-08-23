(defproject stratege "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts ^:replace ["-server"]
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/tools.macro "0.1.2"]
                 [fast-zip "0.6.1"]
                 [criterium "0.4.3"]
                 [clj-tuple "0.2.2"]])
