(ns jada.core
  (:require [jada.routes :refer :all]
            [monger.core :as mg]
            [taoensso.timbre :as timbre :refer [info error]]
            [ns-tracker.core :refer [ns-tracker]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.adapter.jetty :refer [run-jetty]])
  (:import [com.mongodb MongoOptions ServerAddress]
           [java.nio.file FileSystems StandardWatchEventKinds]))

(defn- auto-reload* [dirs]
  (try (let [modified-namespaces (ns-tracker dirs)
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
             (.reset key))))
       (catch Exception e
         (error (.printStackTrace e)))
       (finally
         (info "Restarting autoloading for " dirs)
         (auto-reload* dirs))))

(defn auto-reload
  [dirs]
  (.start (Thread. (partial auto-reload* dirs))))

(def handler
  (-> app
      (wrap-keyword-params)
      (wrap-params)))

(defn -main []
  (mg/connect!)
  (mg/set-db! (mg/get-db "jada"))
  (auto-reload ["src/jada"])
  (run-jetty #'handler {:port 8081}))
