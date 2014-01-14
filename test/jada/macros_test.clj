(ns jada.macros-test
  (:require [clj-time.core :as t]
            [jada.log :as log]
            [jada.macros :as m]
            [jada.food :as food])
  (:use clojure.test))

(def m (m/create 1000 100 50 60 15))
(def f (assoc (m/create 0 0 0 0 0) :name ""))
(def f2 (assoc (m/create 2000 100 500 600 150) :name ""))
(def plans {"lg" {:rest m}})
(def l {(t/today-at-midnight) {:kind :rest :plan "lg" :eaten []}})

(testing "remaining"
    (deftest with-nothing-eaten-returns-m
      (with-redefs [log/eaten (constantly f)
                    m/today (constantly m)]
        (is (= (m/remaining l plans)
               m))))
  (deftest can-return-negative-values
    (with-redefs [log/eaten (constantly f2)
                  m/today (constantly m)]
      (is (= (m/remaining (food/mult f2 -1) plans))))))
