(ns jada.handler
  (:require [compojure.core :refer :all]
            [compojure.handler :as handler]
            [compojure.route :as route]
            [jada.views :refer :all]))

(def a (atom 42))

(defroutes app-routes
  (GET "/" [] (index-page))
  (POST "/input" {{:keys [input]} :params} (reset! a input))
  (GET "/val" [] (pr-str @a))
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (handler/site app-routes))
