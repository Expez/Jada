(ns jada.core
  (:require [cheshire.core :refer :all]
            [jada.food]
            [jada.handler :refer :all]
            [jada.html :as html]
            [jada.log]
            [monger.core :as mg]
            [ns-tracker.core :refer [ns-tracker]]
            [taoensso.timbre :as timbre :refer [info]])
  (:import [com.mongodb MongoOptions ServerAddress]
           [java.nio.file FileSystems StandardWatchEventKinds]
           [org.webbitserver WebServer WebServers WebSocketHandler HttpHandler]
           [org.webbitserver.handler StaticFileHandler]))

(defn on-message [connection json-message]
  (let [message (parse-string json-message true)
        reply (handle message)]
    (info "Received message" message)
    (.send connection (generate-string reply))))

(defn static-html-handler [html]
  "Creates a handler that serves up static HTML."
  (reify HttpHandler
    (handleHttpRequest [this request response control]
      (doto response
        (.header "Content-type" "text/html")
        (.content html)
        (.end)))))

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
            (require ns-sym :reload)))
        (.reset key)))))

(defn auto-reload
  [dirs]
  (.start (Thread. (partial auto-reload* dirs))))

(defn -main []
  (mg/connect!)
  (mg/set-db! (mg/get-db "jada"))
  (auto-reload ["src/jada"])
  (doto (WebServers/createWebServer 8080)
    (.add "/websocket"
          (proxy [WebSocketHandler] []
            (onOpen [c] (println "opened" c))
            (onClose [c] (println "closed" c))
            (onMessage [c j] (on-message c j))))
    (.start)))
