(ns jada.core
  (:require [cheshire.core :refer :all]
            [jada.food]
            [jada.handler :refer :all]
            [jada.html :as html]
            [jada.log]
            [monger.core :as mg]
            [taoensso.timbre :as timbre :refer [info]]
            [ns-tracker.core :refer [ns-tracker]]
            [liberator.core :refer [resource defresource]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.adapter.jetty :refer [run-jetty]]
            [compojure.core :refer [defroutes ANY]])
  (:import [com.mongodb MongoOptions ServerAddress]
           [java.nio.file FileSystems StandardWatchEventKinds]))

(defn- auto-reload* [dirs]
  (let [modified-namespaces (ns-tracker dirs)
        fs (FileSystems/getDefault)
        watcher (.newWatchService fs)
        events (into-array [StandardWatchEventKinds/ENTRY_MODIFY])
        strs (into-array String [])]
    (doseq [dir dirs]
      (.register (.getPath fs dir strs) watcher events))
    (while true
      (let [key (.take watcher)
            events (.pollEvents key)]
        (if (not-every? #(= (.kind %) StandardWatchEventKinds/OVERFLOW) events)
          (doseq [ns-sym (modified-namespaces)]
            (require ns-sym :reload)
            (info "Reloaded " ns-sym)))
        (.reset key)))))

(defn auto-reload
  [dirs]
  (.start (Thread. (partial auto-reload* dirs))))

(defresource parametrized [x]
  :available-media-types ["text/html"]
  :handle-ok (fn [ctx] (prn-str  "parameter: " x)))

(defroutes app
  #_(ANY "/foo" [] (resource :available-media-types ["text/html"]
                             :handle-ok (fn [ctx] (prn-str "x: " (get-in ctx [:request :params :x])))))
  (ANY "/foo/:x" [x] (parametrized x))  ;This matches localhost/foo/bar/
  (ANY "/foo" [] (resource :available-media-types ["text/html"]
                             :handle-ok (fn [_] "hi"))))

(def handler
  (-> app
      (wrap-keyword-params)
      (wrap-params)))

(defn -main []
  (auto-reload ["src/jada"])
  (run-jetty #'handler {:port 8081}))
