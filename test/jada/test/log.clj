(ns jada.test.log
  (:use [jada.log :as log]
        jada.test.core
        clojure.test)
  (:require [clj-time.core :as t])
  (:import [jada.log LogEntry]))

(def today (t/today-at-midnight))

(testing  "weight"
  (deftest weight-is-updated
    (is (= (:weight ((log/weight {} today 87) today)) 87))
    (is (= (:weight ((log/weight {} 87) today)) 87))))
