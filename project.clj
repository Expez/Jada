(defproject jada "0.1.0-SNAPSHOT"
  :description "Jada is just another diet aid"
  :url "https://github.com/expez/jada"
  :license {:name "MIT" :url "http://opensource.org/licenses/MIT" }
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [compojure "1.1.6"]
                 [ring/ring-json "0.2.0"]
                 [hiccup "1.0.4"]
                 [clj-time "0.6.0"]]
  :plugins [[lein-ring "0.8.8"]]
  :ring {:handler jada.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring-mock "0.1.5"]]}})
