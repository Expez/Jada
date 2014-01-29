(ns jada.html
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [hiccup.element :refer :all]
            [hiccup.form :refer :all]
            [hiccup.util :refer :all]
            [jada.food :as food]
            [jada.handler :refer :all]))

(declare index-body head weight-form weight food-form food-list food food-item)

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
   [:div {:id "add-food"}
    [:form {:id "food-form" :role "form"}
     (input-field "food-name" "Name: ")
     (input-field "food-kcal" "Kcal: ")
     (input-field "food-prot" "Prot: ")
     (input-field "food-fat" "Fat: ")
     (input-field "food-carbs" "Carbs: ")
     (input-field "food-fiber" "Fiber: ")
     [:input {:type "submit" :value "Add food" :class "btn btn-default"}]]]))

(defn food []
  [:div {:id "food"}
   (food-list)
   [:div {:id "food-item"}]])

(defn- food-list []
  (unordered-list {:id "food-list"} (map :name (food/list-all))))

(defn food-item [food]
  (html
   [:h3 (:name food)]
   [:table
    [:tr
     [:td "Kcal: "]
     [:td (:kcal food)]]
    ]))

(defmethod handle "food-item"
  [{:keys [name]}]
  (let [food (food/lookup name)]
    {:action :replace
     :recipient "#food-item"
     :html (food-item food)}))
