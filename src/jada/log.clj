(ns jada.log
  (:require [clj-time.core :as t]
            [clj-time.format :as tf]
            [jada.util :as util]
            [jada.food :as f]
            [monger.core :as mg]
            [monger.collection :as mc]
            [monger.joda-time]))

;;; a log is of the form {clj-time.DateTime <LogEntry>}.
;;; a logentry has the following keys: weight, foods, bmr, plan and kind.
;;; foods is a seq of [food-name amount]
;;; plan indicates the diet plan we're following e.g leangains, warriordiet etc
;;; kind is a keyword indicating what kind of day this is e.g. :training or
;;; :rest, these are often plan specific

(defn entry-for
  "Returns the log entry for `date'"
  [date]
  (let [entry (mc/find-maps "log" {:date date})]
    (if-not (empty? entry)
      (first entry)
      nil)))

(defn- new
  "Creates a new log entry for `date' or for today.  Using default
values from yesterday"
  ([] (empty (t/today-at-midnight)))
  ([date]
     (let [yesterday (entry-for (t/minus (t/today-at-midnight) (t/days 1)))
           entry {:weight 0
                  :foods []
                  :bmr (:bmr yesterday)
                  :plan (:plan yesterday)
                  :kind (:kind yesterday)
                  :date date}]
       (mc/insert-and-return "log" entry))))

(defn weight
  "Sets the weight for the given `date', or if only two arguments are
provided the weight of the most recent entry in the log."
  ([log date weight]
     (assoc-in log [date :weight] weight))
  ([log weight]
     (assoc-in log [(t/today-at-midnight) :weight] weight))
  ([weight] (throw (RuntimeException. "Not implemented"))))

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

(defn today []
  "Gets the entry in the log representing today."
  (entry-for (t/today-at-midnight)))

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

(defn update-in! [date [k & ks] fn & args]
  "Updates the log entry for `date'"
  (let [entry (entry-for date)
        new-entry (update-in entry (list* k ks) fn args)]
    (mc/update "log" new-entry)))
