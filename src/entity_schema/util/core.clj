(ns entity-schema.util.core)


(defn flatten-for-key [m key]
  "Flatten all paths ending in the specified key.

  E.g.

  (flatten-for-key {:a 1
                    :b {:a 2
                        :c 3}
                    :d 4} :a) => {[:a] 1 [:b :a] 2}"

  (letfn [(flatten-upto-key* [a p ks m]
            (if (= p key)
              (assoc a ks m)
              (if (and (map? m) (not (empty? m)))
                (reduce into (map (fn [[k v]] (flatten-upto-key* a k (conj ks k) v)) (seq m)))
                a)))]
    (if (empty? m) m (flatten-upto-key* {} nil [] m))))





(defn flatten-for-key2 [m key & ks]
  "Flatten all paths ending in the specified key.

  E.g.

  (flatten-for-key {:a 1
                    :b {:a 2
                        :c 3}
                    :d 4} :a) => {[:a] 1 [:b :a] 2}"

  (let [keys (into #{key} ks)]
    (letfn [(flatten-upto-key* [a p ks m]
              (if (contains? keys p)
                (let [path (subvec ks 0 (- (count ks) 1))]
                  (if-let [res (get #spy/p a path)]
                    (assoc a path (assoc res p m))
                    (assoc a path {p m})))
                (if (and (map? m) (not (empty? m)))
                  (reduce into (map (fn [[k v]] (flatten-upto-key* a k (conj ks k) v)) (seq m)))
                  a)))]
      (if (empty? m) m (flatten-upto-key* {} nil [] m)))))

(flatten-for-key2 {:a 1
                   :b {:a 2
                       :c 3}
                   :d 4}

                  :a :c)


(defn flatten-keys-2 [m]
  (letfn [(flatten-keys2* [current path value]
            (if (map? value)
              (reduce
                into
                (map (fn [[k v]] (flatten-keys2* current (conj path k) v)) value))
              (conj current [path value])))]
    (flatten-keys2* [] [] m)))

(let [keys #{:db/id :db/ident}
      input {:db/id 1
             :b     {:db/id    4
                     :db/ident :ident}
             :d     4}
      grouped (->> (flatten-keys-2 input)
                   (group-by (fn [[k _]]
                               (let [subvec (subvec k 0 (- (count k) 1))
                                     p (last k)]
                                 (if (not (contains? keys p))
                                   -1
                                   subvec)))))]
  (->> (dissoc grouped -1)
       (map (fn [[k values]]
              [k (->> values
                      (map (fn [[path v]]
                        [(last path) v]))
                      (into {}))]))))


(take-last (count [:1]) [:a])

(take-last (- (count [1 2 3 4 5]) 1) [1 2 3 4 5])



{[] [[[:db/id] 1]], [:b] [[[:b :db/id] 4] [[:b :db/ident] :ident]]}

(defn flatten-for-keys [m k & ks]
  "Flatten all paths ending in the specified key.

  E.g.

  (flatten-for-key {:a 1
                    :b {:a 2
                        :c 3}
                    :d 4} :a) => {[:a] 1 [:b :a] 2}"

  (let [keys (into #{k} ks)]
    (letfn [(flatten-upto-key* [a p ks m]
              (if (contains? keys p)
                (assoc a ks m)
                (if (and (map? m) (not (empty? m)))
                  (reduce into (map (fn [[k v]] (flatten-upto-key* a k (conj ks k) v)) (seq m)))
                  a)))]
      (if (empty? m) m (flatten-upto-key* {} nil [] m)))))


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
  {:added  "1.0"
   :static true}
  [m [k & ks]]
  (if ks
    (assoc m k (dissoc-in (get m k) ks))
    (dissoc m k)))


