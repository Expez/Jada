(ns jada.routes
  (:require [cheshire.core :refer :all]
            [compojure.core :refer [defroutes ANY]]
            [compojure.route :as route]
            [jada.food :as food]
            [liberator.core :refer [resource defresource]]
            [ring.util.response :as response]))

(defresource food [name]
  :allowed-methods [:put :get :delete]
  :available-media-types ["application/json" "text/html " "text/plain"]
  :exists? (fn [_] (food/lookup name))
  :handle-ok (fn [_] (generate-string (food/lookup name)))
  :can-put-to-missing true
  :put! (fn [ctx] (let [food (-> ctx
                                 (get-in [:request :body])
                                 slurp
                                 (parse-string true)
                                 (:food))]
                    (food/put food)))
  :delete! (fn [_] (food/delete name)))

(defresource list-all-foods []
  :available-media-types ["application/json" "text/html " "text/plain"]
  :handle-ok (fn [ctx] (generate-string {:foods (food/list-all)})))

(defroutes app
  (ANY "/" [] (response/resource-response "index.html" {:root "."}))
  (ANY "/foods/:name" [name] (food name))
  (ANY "/foods" [] (list-all-foods))
  (route/resources "/" {:root "."})
  (ANY "*" [] (println "Not found")))
