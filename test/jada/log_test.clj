(ns jada.log-test
  (:require [jada.log :as log]
            [jada.food :as f]
            [clj-time.core :as t]
            [cheshire.core :refer :all]
            [monger.core :as mg]
            [monger.collection :as mc]
            [monger.joda-time]
            [clojure.test :refer :all]))

(defn with-test-db [f]
  "Connects and disconnects to a fresh test db."
  (mg/connect!)
  (mg/use-db! "test")
  (f)
  (mg/disconnect!))

(defn with-temporary-entries [f]
  "Snapshots the db, performs f, then loads the snapshot."
  (let [all-records (mc/find-maps "log")]
    (f)
    (mc/remove "log")
    (mc/insert-batch "log" all-records)))

(use-fixtures :once with-test-db)
(use-fixtures :each with-temporary-entries)

(def today (t/today-at-midnight))
(def cookies (f/create "cookies" 1 2 3 4 5))

(testing  "weight"
  (deftest weight-with-date
    (log/weight 87)
    (is (= (:weight (log/today)) 87)))
  (deftest weight-without-date
    (log/weight today 87)
    (is (= (:weight (log/today)) 87))))

(testing "ate"
  (deftest food-is-appended
    (log/ate today cookies)
    (is (= (:foods (log/today))
           [[cookies 1]])))

  (deftest food-is-appended-with-correct-amount
    (log/ate today cookies 2)
    (is (= (:foods (log/today))
           [[cookies 2]]))))

(testing "barf"
  (deftest food-is-removed
    (log/ate today cookies 2)
    (log/barfed today cookies 2)
    (is (= (:foods (log/today))
           []))))

(deftest eaten
  (log/ate today cookies)
  (log/ate today cookies)
  (is (= (log/eaten)
         (f/create "" 2 4 6 8 10))))
