(ns jada.macros)

(defn new-plan [plans name kinds]
  "Add new diet plan by `name'.  `kinds' is a map of {:day kcal-diff},
  e.g {:rest -10, :training 30} which indicates that the rest days
  should be at -10% of base kcal, and the traning day at +30%."
  (assoc plans name kinds))

(defn get-plan [plans name]
  (:name plans))
