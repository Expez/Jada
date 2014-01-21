(ns jada.core
  (:require [cheshire.core :refer :all]
            [jada.handlers]
            [monger.core :as mg]
            [jada.html :as html])
  (:import [org.webbitserver WebServer WebServers WebSocketHandler HttpHandler]
           [org.webbitserver.handler StaticFileHandler]
           [com.mongodb MongoOptions ServerAddress]))

(defn on-message [connection json-message]
  (let [message (parse-string json-message true)
        f (:fn message)
        args (list (:args message) (:sender message) )
        handler (resolve (symbol "jada.handlers" f))]
    (when handler
      (.send connection (generate-string (apply handler args))))))

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
