(ns jada.core
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [jada.html :as html])
  (:import [org.webbitserver WebServer WebServers WebSocketHandler HttpHandler]
           [org.webbitserver.handler StaticFileHandler]))

(defn foo [& args]
  [pr-str args])

(defn on-message [connection json-message]
  (let [message (json/read-json json-message)
        f (:fn message)
        args (:args message)]
    (prn "message " message "f " f "args " args)
    (.send connection (apply (resolve (symbol f)) args))))

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
