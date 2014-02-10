(ns jada.routes
  (:require [cheshire.core :refer :all]
            [compojure.core :refer [defroutes ANY]]
            [jada.food :as food]
            [liberator.core :refer [resource defresource]]))

(defresource food [name]
  :allowed-methods [:put :get :delete]
  :available-media-types ["application/json" "text/html " "text/plain"]
  :exists? (fn [_] (food/lookup name))
  :handle-ok (fn [_] (generate-string (food/lookup name)))
  :can-put-to-missing true
  :put! (fn [ctx] (let [f (get-in ctx [:request :params])]
                    (food/put f)))
  :delete! (fn [_] (food/delete name)))

(defresource list-all-foods []
  :available-media-types ["application/json" "text/html " "text/plain"]
  :handle-ok (fn [ctx] (generate-string (food/list-all))))

(defroutes app
  (ANY "/food/:name" [name] (food name))
  (ANY "/food" [] (list-all-foods))
  (ANY "*" [] (println "Not found")))
