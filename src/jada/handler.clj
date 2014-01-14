(ns jada.handler
  (:require [compojure.core :refer :all]
            [compojure.handler :as handler]
            [compojure.route :as route]
            [jada.views :refer :all]))

(defroutes app-routes
  (GET "/" [] (index-page))
  (POST "/input" {{:keys [input]} :params} (prn-str input))
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (handler/site app-routes))
