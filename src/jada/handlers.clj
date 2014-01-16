(ns jada.handlers
  (:require [jada.html :as html]
            [clojure.data.json :as json]
            [jada.log :as log]))

(defn weight [weight recipient]
  (let [weight (Double/parseDouble weight)]
    (log/save (log/weight (log/get) weight))
    {:action :replace
     :value weight
     :recipient recipient}))