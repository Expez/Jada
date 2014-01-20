(ns jada.html
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]))

(declare index-body head weight-form weight food-form)

(defn index-page []
  (html5
   (head)
   (index-body)))

(defn index-body []
  (html
   [:body
    [:h1 "Jada"]
    (weight-form nil)
    (food-form)
    (include-js "js/jada.js")]))

(defn- head []
  (html
   [:head
    [:title "Jada"]
    (include-css "//netdna.bootstrapcdn.com/bootstrap/3.0.3/css/bootstrap.min.css")
    (include-js "http://code.jquery.com/jquery-2.0.3.min.js"
                "//cdnjs.cloudflare.com/ajax/libs/underscore.js/1.5.2/underscore-min.js"
                "//netdna.bootstrapcdn.com/bootstrap/3.0.3/js/bootstrap.min.js")]))

(defn- weight-form [val]
  (html
   [:form {:id "weight-form"}
    (weight val)
    ]))

(defn weight [val]
  (html
   "Weight:" [:input {:type "text" :value val :id "weight"}]))

(defn- food-form []
  (html
   [:form {:id "food-form"}
    "Name:" [:input {:id "food-name"}]
    "Kcal:" [:input {:id "food-kcal"}]
    "Prot:" [:input {:id "food-prot"}]
    "Fat:" [:input {:id "food-fat"}]
    "Carbs:" [:input {:id "food-carbs"}]
    "Fiber:" [:input {:id "food-fiber"}]
    [:input {:type "submit" :value "Add food"}]]))
