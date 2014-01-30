(defproject jada "0.1.0-SNAPSHOT"
  :main jada.core
  :description "Jada is just another diet aid"
  :url "https://github.com/expez/jada"
  :license {:name "MIT" :url "http://opensource.org/licenses/MIT" }
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [cheshire "5.3.1"]
                 [com.taoensso/timbre "3.0.0-RC4"]
                 [hiccup "1.0.4"]
                 [com.novemberain/monger "1.7.0"]
                 [org.webbitserver/webbit "0.4.15"]
                 [org.clojure/tools.trace "0.7.6"]
                 [clj-time "0.6.0"]
                 [org.clojure/tools.namespace "0.2.1"]
                 [ns-tracker "0.2.1"]])
