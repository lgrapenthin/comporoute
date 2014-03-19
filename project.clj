(defproject comporoute "0.1.0"
  :description "Composable request handling."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [com.stuartsierra/component "0.2.1"]
                 [ring/ring-codec "1.0.0"]
                 [clout "1.1.0"]]
  :profiles {:dev {:source-paths ["dev"]}})
