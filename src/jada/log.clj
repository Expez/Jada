(ns jada.log
  (:require [clj-time.core :as t])
  (:require [jada.food :refer :all])
  (:import [jada.food Food]))

(defn weight
  "Sets the weight for the given `date', or if only two arguments are
provided the weight of the most recent entry in the log."
  ([log date weight ]
     (assoc-in log [date :weight] weight))
  ([log weight]
     (assoc-in log [(t/today-at-midnight) :weight] weight)))

(defn ate
  "Log that we ate `food', or that we ate `amount' of `food'."
  ([log food]
     (ate log food 1))
  ([log food amount]
     (update-in log [(t/today-at-midnight) :foods] conj [food amount])))

(defn barfed
  "The reverse the eating of `food'."
  ([log food]
     (barfed log food 1))
  ([log food amount]
     (update-in log [(t/today-at-midnight) :foods]
                (partial remove #{[food amount]}))))

(defn- today [log]
  "Gets the entry in the log representing today."
  (or (log (t/today-at-midnight))
      {}))

(defn aggregate [log date]
  "Tallies up all the food items, returns a new aggregate `Food'."
  (if-let [day (log date)]
    (reduce add (map #(mult (first %) (second %)) (:foods day)))
    (Food. "" 0 0 0 0 0)))

(defn set-bmr [log bmr]
  "Sets the basal metabolic rate in the log."
  (assoc-in log [t/today-at-midnight :bmr] bmr))
