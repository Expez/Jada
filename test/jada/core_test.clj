(ns jada.core-test
  (:require [jada.core :refer :all]
            [clojure.test :refer :all])
  (:import [jada.core Food]))

(def f1 (Food. "f1" 1000 50 25 75 2))
(def f2 (Food. "f2" 2000 70 5 5 3))
(def f3 (Food. "f3" 100 7 2 1 0))
(def f4 (Food. "" 3100 127 32 81 5))

(deftest add-food
  (is (= f4 (add f1 f2 f3))
      "each component added"))
