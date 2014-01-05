(ns jada.log
  :import [java.util Date])

(defrecord LogEntry [date weight foods])
(def *log* {})

(defprotocol Log
  (save [] "Stores the log through some means of persistance.")
  (load [] "Reads the log back in.")
  (weight [weight date] "Sets the `weight' of the entry matching `date'.")
  (eat [food])
  (eat [food amount] "Log that we ate `amount' of `food'.")
  (barf [food])
  (barf [food amount]) "Undo the eating of `amount' of `food'.")

(defn weight
  "Sets the weight of the LogEntry, or if only one argument is
provided the weight of the most recent LogEntry."
  ([log-entry weight]
     (assoc log-entry :weight weight))
  ([weight]
    (set! *log* (assoc (first *log*) [:weight weight]))
    (save)))
