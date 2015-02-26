(defproject floodr "0.3.0"
  :description "A game for the unix terminal."
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clojure-lanterna "0.9.4"]]
  :main ^:skip-aot floodr.cli.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
