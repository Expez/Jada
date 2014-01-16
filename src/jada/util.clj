(ns jada.util)

(defn map-keys [f m]
  "maps `f' over the keys in the map m."
  (into {} (for [[k v] m] [(f k) v])))

(defn map-vals [f m]
  "maps `f' over the values in the map m."
  (into {} (for [[k v] m] [k (f v)])))
