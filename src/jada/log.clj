(ns jada.log
  (:use [jada.core])
  (:require [clj-time.core :as t]))

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
