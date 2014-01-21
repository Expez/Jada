(ns jada.handlers
  (:require [jada.html :as html]
            [jada.log :as log]))

(defn weight [weight recipient]
  (let [weight (Double/parseDouble weight)]
    (log/weight weight)
    {:action :replace
     :value weight
     :recipient recipient}))

(defn add-food [{:keys [name kcal prot fat carbs fiber]} recipient]
  {:action :none
   :recipient :none})
