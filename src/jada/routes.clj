(ns jada.routes
  (:require [cheshire.core :refer :all]
            [clostache.parser :as clostache]
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
  :put! (fn [ctx] (let [f (get-in ctx [:request :params])]
                    (food/put f)))
  :delete! (fn [_] (food/delete name)))

(defresource list-all-foods []
  :available-media-types ["application/json" "text/html " "text/plain"]
  :handle-ok (fn [ctx] (generate-string {:foods (food/list-all)})))

(defn render-template [template-file]
   (clostache/render (slurp template-file) nil))

(defn index []
  (render-template "jada-web/index.html"))

(defroutes app
  (route/resources "/" {:root "."})
  (ANY "/foods/:name" [name] (food name))
  (ANY "/foods" [] (list-all-foods))
  (ANY "*" [] (println "Not found")))
