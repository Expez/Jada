(ns jada.util)

(defn map-keys [f m]
  "maps `f' over the keys in the map m."
  (into {} (for [[k v] m] [(f k) v])))

(defn map-vals [f m]
  "maps `f' over the values in the map m."
  (into {} (for [[k v] m] [k (f v)])))

(defn keep-keys [m keys]
  "Filters out any keys from m which isn't listed in the seq keys."
  (let [keep-keys (into #{} keys)]
    (into {} (filter #(keep-keys (first %)) m))))
