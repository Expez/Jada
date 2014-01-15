(ns jada.html
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]))

(declare index-body head)

(defn index-page []
  (html5
   (head)
   (index-body)))

(defn index-body []
  (html
   [:body
    [:h1 "Websocket demo"]
    [:input {:type "text" :id "box"}]]))

(defn- head []
  (html
   [:head [:title "Jada"]
    (include-css "/css/jada.css")
    (include-js "js/jada.js"
                "http://code.jquery.com/jquery-2.0.3.min.js")]))
