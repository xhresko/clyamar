(defproject clyamar "0.2.0"
  :description "Simple forest learner for Clojure."
  :url "https://github.com/xhresko/clyamar"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.cli "0.3.5"]]
  :main ^:skip-aot clyamar.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
