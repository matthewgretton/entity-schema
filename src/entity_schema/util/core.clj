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
              (if (map? m)
                (reduce into (map (fn [[k v]] (flatten-upto-key* a k (conj ks k) v)) (seq m)))
                a)))]
    (flatten-upto-key* {} nil [] m)))
