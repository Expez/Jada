(ns jada.log-test
  (:require [jada.log :refer :all]
            [jada.core]
            [clj-time.core :as t]
            [clojure.test :refer :all])
  (:import [jada.core Food]))


(def today (t/today-at-midnight))
(def cookies (Food. "cookies" 1 2 3 4 5))

(testing  "weight"
  (deftest weight-is-updated
    (is (= (:weight ((weight {} today 87) today)) 87))
    (is (= (:weight ((weight {} 87) today)) 87))))

(testing "eat"
  (deftest food-is-appended
    (is (= (:foods ((ate {} cookies) today)) [[cookies 1]])))

  (deftest food-is-appended-with-correct-amount
    (is (= (:foods ((ate {} 2 cookies) today))) [[cookies 2]])))
