(ns jada.macros-test
  (:require [clj-time.core :as t]
            [jada.log :as log]
            [jada.macros :as m])
  (:use clojure.test))

(def m (m/create 1000 100 50 60 15))
(def f (assoc (m/create 1000 100 50 60 15) :name ""))
(def plans {"lg" {:rest m}})
(def l {(t/today-at-midnight) {:kind :rest :plan "lg" :eaten []}})

(deftest remaining
  (with-redefs [log/eaten (constantly f)
                m/today (constantly m)]
    (is (= (m/remaining l plans)
           m))))
