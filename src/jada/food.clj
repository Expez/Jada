(ns jada.food
  (:require [jada.util :as util]))

;;; a food is a map with the following keys: name kcal prot fat carbs fiber

(defn combine-with [f m1 m2]
  "Given {:k v1 ...} and {:k v2 ...} returns {:k f(v1 v2) ...}"
  (into {} (for [[k v1] m1
                 :let [v2 (k m2)]
                 :when v2]
             [k (f v1 v2)])))

(defn add
  "Add Food items"
  [& foods]
  (reduce (partial combine-with #(if (number? %1) (+ %1 %2) "")) foods))

(defn mult [food amount]
  (util/map-vals #(if (number? %) (* amount %) %) food))

(defn new-food [foods food]
  (assoc foods (:name food) food))

(defn lookup [foods name]
  (:name foods))

(defn create [name kcal prot fat carbs fiber]
  {:name name :kcal kcal :prot prot :fat fat :carbs carbs :fiber fiber})

(defn diff [f1 f2]
  "Returns f1 - f2"
  (add (util/map-vals #(if (number? %) (* -1 %) "") f2) f1))
