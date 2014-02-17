(ns jada.food
  (:require [jada.util :as util]
            [monger.collection :as mc]
            [taoensso.timbre :as timbre :refer [info error]]))

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

(defn- validate-food [food]
  (and (map? food)
       (= (count (keys food)) 6)
       (every? #{:name :kcal :fat :prot :carbs :fiber} (keys food))
       (every? #(not (nil? %)) (vals food))))

(defn lookup [name]
  (dissoc (mc/find-one-as-map "foods" {:name name}) :_id ))

(defn delete [name]
  (mc/remove "foods" {:name name}))

(defn put [food]
  "Add a new food item to the db."
  (if-not (validate-food food)
    (error "Tried to PUT " food)
    (info "PUT: "
          (when (lookup (:name food))
            (delete (:name food)))
          (mc/insert-and-return "foods" food))))

(defn create [name kcal prot fat carbs fiber]
  {:name name :kcal kcal :prot prot :fat fat :carbs carbs :fiber fiber})

(defn diff [f1 f2]
  "Returns f1 - f2"
  (add (util/map-vals #(if (number? %) (* -1 %) "") f2) f1))

(defn list-all []
  "Returns a map of all foods in the db"
  (map #(dissoc % :_id) (mc/find-maps "foods")))
