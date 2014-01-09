(ns jada.test.log
  (:use [jada.log :as log]
        jada.test.core
        clojure.test)
  (:require [clj-time.core :as t]
            [jada.core Food]))

(def today (t/today-at-midnight))
(def cookies (Food. "cookies" 1 2 3 4 5))

(testing  "weight"
  (deftest weight-is-updated
    (is (= (:weight ((log/weight {} today 87) today)) 87))
    (is (= (:weight ((log/weight {} 87) today)) 87))))

(testing "eat"
  (deftest food-is-appended
    (is (= (:foods ((log/ate {} cookies) today)) [[cookies 1]])))

  (deftest food-is-appended-with-correct-amount
    (is (= (:foods ((log/ate {} 2 cookies) today))) [[cookies 2]])))
