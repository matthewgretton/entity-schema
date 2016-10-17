(ns entity-schema.util.core)


(defn flatten-for-key [m key]
  "Flatten all paths ending in the specified key.

  E.g.

  (flatten-for-key {:a 1
                    :b {:a 2
                        :c 3}
                    :d 4} :a) => {:a 1 [:b :a] 2}"

  (letfn [(flatten-upto-key* [a p ks m]
            (if (= p key)
              (assoc a ks m)
              (if (and (map? m) (not (empty? m)))
                (reduce into (map (fn [[k v]] (flatten-upto-key* a k (conj ks k) v)) (seq m)))
                a)))]
    (if (empty? m) m (flatten-upto-key* {} nil [] m))))


(defn flatten-keys [m]
  "Flatten all paths ending in the specified key.

  E.g.

  (flatten-for-key {:a 1
                    :b {:a 2
                        :c 3}
                    :d 4} :a) => {:a 1 [:b :a] 2}"

  (letfn [(flatten-keys* [a ks m]
            (if (and (map? m) (not (empty? m)))
              (reduce into (map (fn [[k v]] (flatten-keys* a (conj ks k) v)) m))
              (assoc a ks m)))]
    (if (empty? m) m (flatten-keys* {} [] m))))



(defn dissoc-in
  "Dissociates a value in a nested associative structure, where ks is a
  sequence of keys and v is the new value and returns a new nested structure.
  If any levels do not exist, hash-maps will be created."
  {:added "1.0"
   :static true}
  [m [k & ks]]
  (if ks
    (assoc m k (dissoc-in (get m k) ks))
    (dissoc m k)))


