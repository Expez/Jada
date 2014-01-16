(ns jada.html
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]))

(declare index-body head box inner-box)

(defn index-page []
  (html5
   (head)
   (index-body)))

(defn index-body []
  (html
   [:body
    [:h1 "Jada"]
    (box nil)
    (include-js "js/jada.js")]))

(defn- head []
  (html
   [:head
    [:title "Jada"]
    (include-js "http://code.jquery.com/jquery-2.0.3.min.js")]))

(defn- box [val]
  (html
   [:form {:id "box"}
    (inner-box val)
    ]))

(defn inner-box [val]
  (html
   [:input {:type "text" :value val :id "input"}]
   [:input {:type "submit" :value "Submit"}]))
