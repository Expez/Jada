(ns jada.core
  (:require [jada.routes :only [app]]))

(defn -main [port]
  (run-jetty app {:port (Integer. port)}))
