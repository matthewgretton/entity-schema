(ns entity-schema.id-transform-gist)

;; Functions for dealing with a bi-directional map modelled as a pair of maps. The first map, mapping to value,
;; and the second mapping value to key [kv vk]

(defn bimap-create-empty []
  [{} {}])

(defn bimap-contains-value? [[_ vk] v]
  (contains? vk v))

(defn bimap-get-value [[kv _] k]
  (get kv k))

(defn bimap-contains-key? [[kv _] k]
  (contains? kv k))

(defn bimap-get-key [[_ vk] v]
  (get vk v))

(defn bimap-consistently-mapped? [m k v]
  (let [mapped-v (bimap-get-value m k)
        mapped-k (bimap-get-key m v)]
    (and (= k mapped-k) (= v mapped-v))))

(defn bimap-consistently-unmapped? [m k v]
  (let [mapped-k (bimap-get-value m k)
        mapped-v (bimap-get-key m v)]
    (and (nil? mapped-k) (nil? mapped-v))))

(defn bimap-not-contains? [m k v]
  (and (not (bimap-contains-key? m k)) (not (bimap-contains-value? m v))))

(defn bimap-assoc [[kv vk] k v]
  "Assocs the mapping. If a different but overlapping mapping aleady exists it will throw an exception... (or possibly)"
  (assert (bimap-not-contains? [kv vk] k v))
  [(clojure.core/assoc kv k v) (clojure.core/assoc vk v k)])


;; Core helper functions
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

;; Actual code for comparing transaction data
(defn is-transacted-db-id? [id]
  "Is this a transacted db id"
  (or (integer? id) (keyword? id)))

(defn is-temp-db-id? [id]
  "Is this a temp db id?"
  (and (map? id) (contains? id :part) (contains? id :idx)))



(require 'spyscope.core)

(defn error
  ([type]
   {:error/type type})
  ([type data]
   (assoc (error type) :error/data data)))

(defn make-entity-consistent
  "Make actual entity ids consistent with expected entity ids"
  ([actual-entity expected-entity]
   (make-entity-consistent actual-entity expected-entity (bimap-create-empty)))
  ([actual-entity expected-entity input-exp-act-bimap]
   (->> (flatten-for-key expected-entity :db/id)
        (reduce (fn [[exp-act-bimap output] [expected-id-path expected-id]]
                  (if-let [actual-id (get-in actual-entity expected-id-path)]
                    (cond

                      ;; consistently unmapped, but partitions are the same
                      (and (bimap-consistently-unmapped? exp-act-bimap expected-id actual-id)
                           (or (and (is-transacted-db-id? actual-id) (= actual-id expected-id))
                               (and (and (is-temp-db-id? actual-id) (is-temp-db-id? expected-id))
                                    (= (:part actual-id) (:part expected-id)))))
                      [(bimap-assoc exp-act-bimap expected-id actual-id) (assoc-in output expected-id-path expected-id)]

                      ;; consistently mapped
                      (bimap-consistently-mapped? exp-act-bimap expected-id actual-id)
                      [exp-act-bimap (assoc-in output expected-id-path expected-id)]

                      ;; there's an inconsistency between the mapping and the inputs
                      :else
                      (let [error (error :error.type/inconsisten-ids {:actual-id     actual-id
                                                                      :expected-id   expected-id
                                                                      :mapped-act-id (bimap-get-value exp-act-bimap expected-id)
                                                                      :mapped-exp-id (bimap-get-key exp-act-bimap actual-id)})]
                        [exp-act-bimap (assoc-in output expected-id-path error)]))
                    [exp-act-bimap (assoc-in output expected-id-path (error :error.type/missing-id))]))
                [input-exp-act-bimap actual-entity]))))

(defn make-all-entities-consistent [actual-entities expected-entities]
  "Make all actual entity ids consistent with the expected entity ids."
  (->> (map vector actual-entities expected-entities)
       (reduce (fn [[exp-act-id-bi-map output-coll] [actual expected]]
                 (let [[new-exp-act-id-bi-map output] (make-entity-consistent actual expected exp-act-id-bi-map)]
                   [new-exp-act-id-bi-map (conj output-coll output)])) [(bimap-create-empty) []])
       (second)))

;;should work fine
(make-all-entities-consistent [{:db/id {:part :db.part/user :idx 122}}
                               {:db/id {:part :db.part/user :idx 122}}]

                              [{:db/id {:part :db.part/user :idx 425}}
                               {:db/id {:part :db.part/user :idx 425}}])

;;stuff
(make-all-entities-consistent [{:db/id {:part :db.part/user :idx 122}}
                               {:db/id {:part :db.part/custom :idx 122}}]

                              [{:db/id {:part :db.part/user :idx 425}}
                               {:db/id {:part :db.part/user :idx 425}}])

;; should fail
(make-all-entities-consistent [{:db/id {:part :db.part/user :idx 122}}
                               {:db/id {:part :db.part/user :idx 121}}]

                              [{:db/id {:part :db.part/user :idx 425}}
                               {:db/id {:part :db.part/user :idx 425}}])

;;should fail
(make-all-entities-consistent [{:db/id {:part :db.part/user :idx 122}}
                               {:db/id {:part :db.part/user :idx 122}}]

                              [{:db/id {:part :db.part/user :idx 424}}
                               {:db/id {:part :db.part/user :idx 425}}])

;; should fail as ids are different
(make-all-entities-consistent [{:db/id :something}]
                              [{:db/id :something2}])
