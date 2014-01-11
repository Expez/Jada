(ns jada.food-test
  (:require [jada.food :refer :all]
            [clojure.test :refer :all]))

(def f1 (create "f1" 1000 50  25 75 2))
(def f2 (create "f2" 2000 70   5  5 3))
(def f3 (create "f3" 100   7   2  1 0))
(def f4 (create ""   3100 127 32 81 5))
(def f5 (create ""   1000 20 -20 -70 1))

(deftest add-food
  (is (= f4 (add f1 f2 f3))
      "each component added"))

(deftest food-difference
  (is (= (diff f2 f1)
         f5)))
