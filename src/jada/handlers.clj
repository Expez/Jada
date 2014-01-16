(ns jada.handlers
  (:require [jada.html :as html]
            [clojure.data.json :as json]))

(defn box [val recipient]
  {:html (json/write-str (html/inner-box val))
   :recipient recipient})
