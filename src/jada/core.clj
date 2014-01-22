(ns jada.core
  (:require [cheshire.core :refer :all]
            [jada.handler :refer :all]
            [taoensso.timbre :as timbre :refer [info]]
            [monger.core :as mg]
            [jada.log]
            [jada.food]
            [jada.html :as html])
  (:import [org.webbitserver WebServer WebServers WebSocketHandler HttpHandler]
           [org.webbitserver.handler StaticFileHandler]
           [com.mongodb MongoOptions ServerAddress]))

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

(defn -main []
  (mg/connect!)
  (mg/set-db! (mg/get-db "jada"))
  (doto (WebServers/createWebServer 8080)
    (.add "/websocket"
          (proxy [WebSocketHandler] []
            (onOpen [c] (println "opened" c))
            (onClose [c] (println "closed" c))
            (onMessage [c j] (on-message c j))))
    (.add "/" (static-html-handler (html/index-page)))
    (.add (StaticFileHandler. "resources"))
    (.start)))
