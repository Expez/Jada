(ns jada.routes
  (:require [cheshire.core :refer :all]
            [compojure.core :refer [defroutes ANY]]
            [jada.food :as food]
            [liberator.core :refer [resource defresource]]))

(defresource foods [name]
  :allowed-methods [:put :get :delete]
  :available-media-types ["application/json" "text/html " "text/plain"]
  :handle-ok (fn [ctx] (generate-string (food/lookup name)))
  :put! (fn [ctx] (println (get-in ctx [:request :params]))))

(defresource list-all-foods []
  :available-media-types ["application/json" "text/html " "text/plain"]
  :handle-ok (fn [ctx] (generate-string (food/list-all))))

(defroutes app
  (ANY "/foo" [] (resource :available-media-types ["text/html"]
                           :handle-ok "<html>Hello, Internet.</html>"))
  (ANY "/foods/:name" [name] (foods name))
  (ANY "/foods" [] (list-all-foods)))
