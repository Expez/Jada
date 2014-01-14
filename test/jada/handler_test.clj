(ns jada.handler-test
  (:require [jada.handler :refer :all]
            [ring.mock.request :refer :all]
            [clojure.test :refer :all]
            [clojure.string :as str]))

(deftest test-app
  (testing "main route"
    (let [response (app (request :get "/"))]
      (is (= (:status response) 200))
      (is (=  (re-find #"Hello World" (:body response) )))))

  (testing "not-found route"
    (let [response (app (request :get "/invalid"))]
      (is (= (:status response) 404)))))
