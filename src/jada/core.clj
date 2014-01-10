(ns jada.core)

(defrecord Food [name kcal prot fat carbs fiber])

(defn add
  "Add Food items"
  [& foods]
  (letfn [(add [{k1 :kcal p1 :prot f1 :fat c1 :carbs fi1 :fiber}
                {k2 :kcal p2 :prot f2 :fat c2 :carbs fi2 :fiber}]
            (Food. "" (+ k1 k2) (+ p1 p2) (+ f1 f2) (+ c1 c2) (+ fi1 fi2)))]
      (reduce add foods)))

(defn mult [{:keys [name kcal prot fat carbs fiber]} amount]
  (Food. name (* amount kcal) (* amount prot) (* amount fat) (* amount carbs)
         (* amount fiber)))
