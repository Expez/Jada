(ns jada.handler)

(defmulti handle
  "Handles messages from the client."
  (fn [message] (:request message)))
