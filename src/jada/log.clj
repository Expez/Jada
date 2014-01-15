(ns jada.log
  (:require [clj-time.core :as t])
  (:require [jada.food :as f])
  (:refer-clojure :exclude [empty]))

;;; a log is of the form {clj-time.DateTime <LogEntry>}.
;;; a logentry has the following keys: weight, foods, bmr, plan and kind.
;;; foods is a seq of [food-name amount]
;;; plan indicates the diet plan we're following e.g leangains, warriordiet etc
;;; kind is a keyword indicating what kind of day this is e.g. :training or
;;; :rest, these are often plan specific

(declare empty)

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

(defn today [log]
  "Gets the entry in the log representing today."
  (or (log (t/today-at-midnight))
      (empty)))

(defn- empty []
  {:weight 0
   :foods []
   :bmr 0
   :plan ""
   :kind nil})

(defn eaten
  "Tallies up all the food items we've eaten on `date' or today."
  ([log] (eaten log (t/today-at-midnight)))
  ([log date]
     (if-let [day (log date)]
       (reduce f/add (map #(f/mult (first %) (second %)) (:foods day)))
       (f/create "" 0 0 0 0 0))))

(defn set-bmr [log bmr]
  "Sets the basal metabolic rate in the log."
  (assoc-in log [t/today-at-midnight :bmr] bmr))
