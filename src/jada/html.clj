(ns jada.html
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [hiccup.element :refer :all]
            [hiccup.form :refer :all]
            [jada.food :as food]))

(declare index-body head weight-form weight food-form food-list)

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
    (food)
    (include-js "js/jada.js")]))

(defn- head []
  (html
   [:head
    [:title "Jada"]
    (include-css "//netdna.bootstrapcdn.com/bootstrap/3.0.3/css/bootstrap.min.css"
                 "css/jada.css")
    (include-js "http://code.jquery.com/jquery-2.0.3.min.js"
                "//cdnjs.cloudflare.com/ajax/libs/underscore.js/1.5.2/underscore-min.js"
                "//netdna.bootstrapcdn.com/bootstrap/3.0.3/js/bootstrap.min.js")]))

(defn- weight-form [val]
  (html
   [:form {:id "weight-form" :role "form" :class "form-inline"}
    (weight val)
    [:input {:type "submit" :value "Set weight" :class "btn btn-default"}]]))

(defn- input-field [id label & [val type]]
  [:div {:class "form-group"}
   [:label {:for id} label]
   [:input {:id id}]])

(defn- weight [val]
  (html
   (input-field "weight" "Weight: " val "text")))

(defn- food-form []
  (html
   [:form {:id "food-form" :role "form" :class "form-inline"}
    (input-field "food-name" "Name: ")
    (input-field "food-kcal" "Kcal: ")
    (input-field "food-prot" "Prot: ")
    (input-field "food-fat" "Fat: ")
    (input-field "food-carbs" "Carbs: ")
    (input-field "food-fiber" "Fiber: ")
    [:input {:type "submit" :value "Add food" :class "btn btn-default"}]]))

(defn food []
  [:div {:id "food"}
   (food-list)
   (food-item)])

(defn- food-list []
  [:div {:id "food-list"}
   (unordered-list (map  :name (food/list-all)))])

(defn- food-item []
  (label "food-item-kcal" "Kcal")
  [:span {:id "food-item-kcal"}])
