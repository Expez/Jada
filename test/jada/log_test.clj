(ns jada.log-test
  (:require [jada.log :as log]
            [jada.food :as f]
            [clj-time.core :as t]
            [monger.core :as mg]
            [monger.collection :as mc]
            [clojure.test :refer :all]))

(defn with-with-test-db [f]
  "Connects and disconnects to a fresh test db."
  (mg/connect!)
  (mg/drop-db (mg/get-db "test"))
  (mg/use-db! "test")
  (f)
  (mg/disconnect!))

(defn with-temporary-entries [f]
  "Snapshots the db, performs f, then loads the snapshot."
  (let [all-records (mc/find-maps "test")]
    (f)
    (mc/remove "test")
    (mc/insert-batch all-records)))

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
    (is (= (:foods ((log/ate {} cookies) today))
           [[cookies 1]])))

  (deftest food-is-appended-with-correct-amount
    (is (= (:foods ((log/ate {} cookies 2) today))
           [[cookies 2]]))))

(testing "barf"
  (deftest food-is-removed
    (is (= (:foods ((log/barfed (log/ate {} cookies 2) cookies 2) today))
           []))))

(deftest food-aggregation
  (is (= (log/eaten(log/ate (log/ate {} cookies) cookies) today)
         (f/create "" 2 4 6 8 10))))

(deftest eaten
  (is (= (log/eaten (log/ate {} cookies))
         cookies)))
