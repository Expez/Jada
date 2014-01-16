(ns jada.core
  (:require [clojure.data.json :as json]
            [jada.handlers]
            [jada.html :as html])
  (:import [org.webbitserver WebServer WebServers WebSocketHandler HttpHandler]
           [org.webbitserver.handler StaticFileHandler]))

(defn on-message [connection json-message]
  (let [message (json/read-json json-message)
        f (:fn message)
        args (list (:args message) (:sender message) )
        handler (resolve (symbol "jada.handlers" f))]
    (prn "message: " message)
    (when handler
      (.send connection (json/write-str (apply handler args))))))

(defn static-html-handler [html]
  "Creates a handler that serves up static HTML."
  (reify HttpHandler
    (handleHttpRequest [this request response control]
      (doto response
        (.header "Content-type" "text/html")
        (.content html)
        (.end)))))

(defn -main []
  (doto (WebServers/createWebServer 8080)
    (.add "/websocket"
          (proxy [WebSocketHandler] []
            (onOpen [c] (println "opened" c))
            (onClose [c] (println "closed" c))
            (onMessage [c j] (on-message c j))))
    (.add "/" (static-html-handler (html/index-page)))
    (.add (StaticFileHandler. "resources"))
    (.start)))
