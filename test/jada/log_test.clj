(ns jada.log-test
  (:require [jada.log :refer :all]
            [jada.food :as f]
            [clj-time.core :as t]
            [clojure.test :refer :all]))


(def today (t/today-at-midnight))
(def cookies (f/create "cookies" 1 2 3 4 5))

(testing  "weight"
  (deftest weight-is-updated
    (is (= (:weight ((weight {} today 87) today)) 87))
    (is (= (:weight ((weight {} 87) today)) 87))))

(testing "ate"
  (deftest food-is-appended
    (is (= (:foods ((ate {} cookies) today))
           [[cookies 1]])))

  (deftest food-is-appended-with-correct-amount
    (is (= (:foods ((ate {} cookies 2) today))
           [[cookies 2]]))))

(testing "barf"
  (deftest food-is-removed
    (is (= (:foods ((barfed (ate {} cookies 2) cookies 2) today))
           []))))

(deftest food-aggregation
  (is (= (aggregate (ate (ate {} cookies) cookies) today)
         (f/create "" 2 4 6 8 10))))
