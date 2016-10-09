(ns entity-schema.util.bimap
  "Functions for dealing with a bi-directional map modelled as a pair of maps
   the first a key -> value map, and the second a value -> key map. [kv vk]")

(defn create-empty []
  [{} {}])


(defn contains-value? [[_ vk] v]
  (contains? vk v))

(defn get-value [[kv _] k]
  (get kv k))

(defn contains-key? [[kv _] k]
  (contains? kv k))

(defn get-key [[_ vk] v]
  (get vk v))

(defn contains? [m k v]
  (and (contains-key? m k) (contains-value? m v)))

(defn consistently-mapped? [m k v]
  (let [mapped-v (get-value m k)
        mapped-k (get-key m v)]
    (and (= k mapped-k) (= v mapped-v))))

(defn consistently-unmapped? [m k v]
  (let [mapped-k (get-value m k)
        mapped-v (get-key m v)]
    (and (nil? mapped-k) (nil? mapped-v))))

(defn not-contains? [m k v]
  (and (not (contains-key? m k)) (not (contains-value? m v))))


(defn assoc [[kv vk] k v]
  "Assocs the mapping. If a different but overlapping mapping aleady exists it will throw an exception... (or possibly)"
  (assert (not-contains? [kv vk] k v ))
  [(clojure.core/assoc kv k v) (clojure.core/assoc vk v k)])
