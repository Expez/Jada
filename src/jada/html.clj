(ns jada.html
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]))

(declare index-body head weight-form weight)

(defn index-page []
  (html5
   (head)
   (index-body)))

(defn index-body []
  (html
   [:body
    [:h1 "Jada"]
    (weight-form nil)
    (include-js "js/jada.js")]))

(defn- head []
  (html
   [:head
    [:title "Jada"]
    (include-js "http://code.jquery.com/jquery-2.0.3.min.js")]))

(defn- weight-form [val]
  (html
   [:form {:id "weight-form"}
    (weight val)
    ]))

(defn weight [val]
  (html
   [:input {:type "text" :value val :id "weight"}]))
