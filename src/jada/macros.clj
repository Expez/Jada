(ns jada.macros
  (:require [jada.log :as log]
            [jada.food :as food]))

(defn new-plan [^String plans name macros]
  "Add new diet plan with `name'.  `macros' is a map of {:kind macros},
  e.g {:rest {:kcal rest-day-kcal ...}."
  (assoc plans name macros))

(defn get-plan [plans name]
  (:name plans))

(defn today [log plans]
  "Returns the macros for today."
  (let [day-kind (:kind (log/today log))
        plan (:plan (log/today log))]
    (get-in plans [plan day-kind])))

(defn create [kcal prot fat carbs fiber]
  (hash-map :kcal kcal :prot prot :fat fat :carbs carbs :fiber fiber))

(defn remaining [log plans]
  "Returns the remaining macros for today."
  (food/diff (today log plans) (log/eaten log)))
