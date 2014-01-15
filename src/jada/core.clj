(ns jada.core
  (:require [clojure.data.json :as json]
            [clojure.string :as str])
  (:import [org.webbitserver WebServer WebServers WebSocketHandler]
           [org.webbitserver.handler StaticFileHandler]))

(defn on-message [connection json-message]
  (let [message (-> json-message json/read-json (:message))]
    (prn json-message)
    (.send connection (json/json-str
                       {:type "downcase" :message (str/upper-case message) }))))

(defn -main []
  (doto (WebServers/createWebServer 8080)
    (.add "/websocket"
          (proxy [WebSocketHandler] []
            (onOpen [c] (println "opened" c))
            (onClose [c] (println "closed" c))
            (onMessage [c j] (on-message c j))))

    (.add (StaticFileHandler. "."))
    (.start)))
