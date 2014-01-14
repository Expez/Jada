(ns jada.views
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]))

(defn index-page []
  (html5
   [:head [:title "Jada"]
    (include-css "/css/jada.css")]
   [:body
    [:h1 "Hello World"]
    [:form {:action "/input" :method "post"}
     [:input {:type "text" :name "input"}]
     [:input {:type "submit" :value "Submit"}]]]))
