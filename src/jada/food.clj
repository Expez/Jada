(ns jada.food
  (:require [jada.util :as util]
            [jada.handler :refer :all]
            [monger.collection :as mc]))

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

(defn insert-food [food]
  (mc/insert-and-return "foods" food))

(defn lookup [name]
  (dissoc (mc/find-one-as-map "foods" {:name name}) :_id ))

(defn create [name kcal prot fat carbs fiber]
  {:name name :kcal kcal :prot prot :fat fat :carbs carbs :fiber fiber})

(defn diff [f1 f2]
  "Returns f1 - f2"
  (add (util/map-vals #(if (number? %) (* -1 %) "") f2) f1))

(defmethod handle "add food"
  [{:keys [food]}]
  (insert-food food))

(defn list-all []
  "Returns a map of all foods in the db"
  (map #(dissoc % :_id) (mc/find-maps "foods")))
