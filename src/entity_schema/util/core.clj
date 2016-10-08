(ns entity-schema.util.core)


(defn flatten-upto-key [m key]
  "Return paths in m up to key, or not at all if not in path.

  (flatten-upto-key {:a 1
                     :b {:a 2
                         :c 3}
                     :d 4} :a) => {:a 1 [:b :a] 2}"

  (letfn [(flatten-upto-key* [a p ks m]
            (if (= p key)
              (assoc a ks m)
              (if (map? m)
                (reduce into (map (fn [[k v]] (flatten-upto-key* a k (conj ks k) v)) (seq m)))
                a)))]
    (flatten-upto-key* {} nil [] m)))
