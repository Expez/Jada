(ns jada.log
  :import [java.util Date])

(defrecord LogEntry [date weight foods])
(def *log* {})

(defprotocol Log
  (save [this] "Stores the log through some means of persistance.")
  (restore [this] "Reads the log back in.")
  (weight [this weight date] "Sets the `weight' of the entry matching `date'.")
  (eat [this food])
  (eat [this food amount] "Log that we ate `amount' of `food'.")
  (barf [this food])
  (barf [this food amount]) "Undo the eating of `amount' of `food'.")

(defn weight
  "Sets the weight for the given `date', or if only two arguments are
provided the weight of the most recent entry in the log."
  ([log weight date]
     (assoc log-entry :weight weight))
  ([weight]
    (set! *log* (assoc (first *log*) [:weight weight]))
    (save)))
