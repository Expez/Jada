(ns jada.log
  (:require [clj-time.core :as t]
            [jada.util :as util]
            [jada.food :as f]
            [monger.core :as mg]
            [monger.collection :as mc]
            [monger.joda-time]))

;;; a logentry has the following keys: weight, foods, bmr, plan and kind.
;;; foods is a seq of [food-name amount]
;;; plan indicates the diet plan we're following e.g leangains, warriordiet etc
;;; kind is a keyword indicating what kind of day this is e.g. :training or
;;; :rest, these are often plan specific

(defn- new-entry
  "Creates a new log entry for `date' or for today.  Using default
values from the previous day."
  ([] (new-entry (t/today-at-midnight)))
  ([date]
     (let [yesterday (mc/find-one-as-map "log" {:date (t/minus
                                                       (t/today-at-midnight)
                                                       (t/days 1))})
           entry {:weight 0
                  :foods []
                  :bmr (:bmr yesterday)
                  :plan (:plan yesterday)
                  :kind (:kind yesterday)
                  :date date}]
       (mc/insert-and-return "log" entry))))

(defn entry-for
  "Returns the log entry for `date'"
  [date]
  (if-let [entry (mc/find-one-as-map "log" {:date date})]
    entry
    (new-entry date)))

(defn update-in! [date [k & ks] fn & args]
  "Updates the log entry for `date'"
  (let [entry (entry-for date)
        new-entry (apply update-in entry (list* k ks) fn args)]
    (mc/update "log" {:date date} new-entry)))

(defn assoc-in! [date [k & ks] v]
  (let [entry (entry-for date)
        new-entry (assoc-in entry (list* k ks) v)]
    (mc/update "log" {:date date} new-entry)))

(defn weight
  "Sets the weight for the given `date', or if only two arguments are
provided the weight of the most recent entry in the log."
  ([w]
     (weight (t/today-at-midnight) w))
  ([date w]
     (assoc-in! date [:weight] w)))

(defn ate
  "Log that we ate `food', or that we ate `amount' of `food'."
  ([date food]
     (ate date food 1))
  ([date food amount]
     (update-in! date [:foods] conj [food amount])))

(defn barfed
  "The reverse the eating of `food'."
  ([date food]
     (barfed date food 1))
  ([date food amount]
     (update-in! date [:foods]
                (partial remove #{[food amount]}))))

(defn today []
  "Gets the entry in the log representing today."
  (entry-for (t/today-at-midnight)))

(defn eaten
  "Tallies up all the food items we've eaten on `date' or today."
  ([] (eaten (t/today-at-midnight)))
  ([date]
     (if-let [foods (:foods (entry-for date))]
       (reduce f/add (map #(f/mult (first %) (second %)) foods))
       (f/create "" 0 0 0 0 0))))

(defn set-bmr [date bmr]
  "Sets the basal metabolic rate in the log, for `date'."
  (assoc-in! date [:bmr] bmr))
