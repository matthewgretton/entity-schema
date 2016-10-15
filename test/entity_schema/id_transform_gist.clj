(ns entity-schema.id-transform-gist
  "Code to check the consistency of transaxion data output.

  It's difficult to test code that prouduces transaction data, as temporary ids are generated randomly. The code in
  this gist provides a way to test the equivalence of two transactions.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for dealing with a bi-directional map modelled as a pair of maps. [{key -> val},{val -> key}]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  "Assocs the mapping. If a different but overlapping mapping aleady exists it will throw an exception..."
  (assert (bimap-not-contains? [kv vk] k v))
  [(clojure.core/assoc kv k v) (clojure.core/assoc vk v k)])




;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Util functions
;;;;;;;;;;;;;;;;;;;;;;;;;;
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






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code for making ids consistent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-transacted-db-id? [id]
  "Is this a transacted db id"
  (or (integer? id) (keyword? id)))

(defn is-temp-db-id? [id]
  "Is this a temp db id?"
  (and (map? id) (contains? id :part) (contains? id :idx)))


(defn error
  ([type]
   {:error/type type})
  ([type data]
   (assoc (error type) :error/data data)))

(require '[spyscope.core])

(defn make-transaction-item-ids-consistent
  "Make actual entity ids consistent with expected entity ids"
  ([actual-entity expected-entity]
   (make-transaction-item-ids-consistent actual-entity expected-entity (bimap-create-empty)))
  ([actual-entity expected-entity input-exp-act-bimap]
   (->> (flatten-for-key expected-entity :db/id)
        (reduce (fn [[exp-act-bimap output] [expected-id-path expected-id]]
                  (if-let [actual-id (get-in actual-entity expected-id-path)]
                    (cond

                      ;; actual and expected ids are consistently unmapped, and ids are already transacted
                      ;; or partitions are the same
                      (and (bimap-consistently-unmapped? exp-act-bimap expected-id actual-id)
                           (or (and (is-transacted-db-id? actual-id) (= actual-id expected-id))
                               (and (and (is-temp-db-id? actual-id) (is-temp-db-id? expected-id))
                                    (= (:part actual-id) (:part expected-id)))))
                      [(bimap-assoc exp-act-bimap expected-id actual-id) (assoc-in output expected-id-path expected-id)]

                      ;; actual and expected ids are consistently mapped
                      (bimap-consistently-mapped? exp-act-bimap expected-id actual-id)
                      [exp-act-bimap (assoc-in output expected-id-path expected-id)]

                      ;; there's an inconsistency between the actual and expected ids.
                      :else
                      (let [error (->> (error :error.type/inconsisten-ids
                                              (->> {:expected-id   expected-id
                                                    :actual-id     actual-id
                                                    :mapped-act-id (bimap-get-value exp-act-bimap expected-id)
                                                    :mapped-exp-id (bimap-get-key exp-act-bimap actual-id)}
                                                   (filter (comp not nil? second))
                                                   (into {})))

                                       (into {}))]
                        [exp-act-bimap (assoc-in output expected-id-path error)]))
                    [exp-act-bimap (assoc-in output expected-id-path (error :error.type/expected-id {:expected-id expected-id}))]))
                [input-exp-act-bimap {}]))))

(defn make-ids-consistent [actual-entities expected-ids]
  "Make all actual entity ids consistent with the expected entity ids."
  (->> (map vector actual-entities expected-ids)
       (reduce (fn [[exp-act-id-bi-map output-coll] [actual expected]]
                 (let [[new-exp-act-id-bi-map output] (make-transaction-item-ids-consistent actual expected exp-act-id-bi-map)]
                   [new-exp-act-id-bi-map (conj output-coll output)])) [(bimap-create-empty) []])
       (second)))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests to show how the make-ids-consistent function works
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require '[clojure.test :refer :all])

(defn test-make-ids-consistent [desc actual expected  expected-output]
  (let [actual-made-consistent (make-ids-consistent actual expected)]
    (is (= expected-output actual-made-consistent) desc)))


(require '[datomic.api :as d])

(deftest making-ids-consistent-tests
  (test-make-ids-consistent "Ids are equivalent"

                            [{:db/id (d/tempid :db.part/user -2)}
                             {:db/id (d/tempid :db.part/user -2)}]

                            [{:db/id (d/tempid :db.part/user -1)}
                             {:db/id (d/tempid :db.part/user -1)}]


                            [{:db/id (d/tempid :db.part/user -1)}
                             {:db/id (d/tempid :db.part/user -1)}])

  (test-make-ids-consistent "Nested Ids are equivalent"

                            [{:db/id      (d/tempid :db.part/user -3)
                              :entity/ref {:db/id (d/tempid :db.part/user -4)}}]

                            [{:db/id      (d/tempid :db.part/user -1)
                              :entity/ref {:db/id (d/tempid :db.part/user -2)}}]



                            [{:db/id      (d/tempid :db.part/user -1)
                              :entity/ref {:db/id (d/tempid :db.part/user -2)}}])

  (test-make-ids-consistent "Nested equivalent ids with same ref"
                            [{:db/id      (d/tempid :db.part/user -3)
                              :entity/ref {:db/id (d/tempid :db.part/user -2)}}]

                            [{:db/id      (d/tempid :db.part/user -1)
                              :entity/ref {:db/id (d/tempid :db.part/user -2)}}]

                            [{:db/id      (d/tempid :db.part/user -1)
                              :entity/ref {:db/id (d/tempid :db.part/user -2)}}])

  (test-make-ids-consistent "Nested equivalent transactions with transacted id"
                            [{:db/id      (d/tempid :db.part/user -3)
                              :entity/ref {:db/id :some-ident}}]

                            [{:db/id      (d/tempid :db.part/user -1)
                              :entity/ref {:db/id :some-ident}}]

                            [{:db/id      (d/tempid :db.part/user -1)
                              :entity/ref {:db/id :some-ident}}])

  (test-make-ids-consistent "Missing Id"
                            [{}]

                            [{:db/id (d/tempid :db.part/user -1)}]

                            [{:db/id {:error/type :error.type/expected-id,
                                      :error/data {:expected-id (d/tempid :db.part/user -1)}}}])

  (test-make-ids-consistent "Expected and actual have inconsistent partitions"

                            [{:db/id (d/tempid :db.part/user -2)}
                             {:db/id (d/tempid :db.part/user -2)}]

                            [{:db/id (d/tempid :db.part/user -1)}
                             {:db/id (d/tempid :db.part/custom -1)}]

                            [{:db/id (d/tempid :db.part/user -1)}
                             {:db/id {:error/data {:actual-id     (d/tempid :db.part/user -2)
                                                   :expected-id   (d/tempid :db.part/custom -1)
                                                   :mapped-exp-id (d/tempid :db.part/user -1)}
                                      :error/type :error.type/inconsisten-ids}}])


  (test-make-ids-consistent "Expected ids different, actual the same"
                            [{:db/id (d/tempid :db.part/user -2)}
                             {:db/id (d/tempid :db.part/user -2)}]

                            [{:db/id (d/tempid :db.part/user -1)}
                             {:db/id (d/tempid :db.part/user -3)}]

                            [{:db/id (d/tempid :db.part/user -1)}
                             {:db/id {:error/data {:actual-id     (d/tempid :db.part/user -2)
                                                   :expected-id   (d/tempid :db.part/user -3)
                                                   :mapped-exp-id (d/tempid :db.part/user -1)}
                                      :error/type :error.type/inconsisten-ids}}])


  (test-make-ids-consistent "Expected ids the same, actual different"
                            [{:db/id (d/tempid :db.part/user -4)}
                             {:db/id (d/tempid :db.part/user -2)}]

                            [{:db/id (d/tempid :db.part/user -1)}
                             {:db/id (d/tempid :db.part/user -1)}]

                            [{:db/id (d/tempid :db.part/user -1)}
                             {:db/id {:error/data {:actual-id     (d/tempid :db.part/user -2)
                                                   :expected-id   (d/tempid :db.part/user -1)
                                                   :mapped-act-id (d/tempid :db.part/user -4)}
                                      :error/type :error.type/inconsisten-ids}}])


  (test-make-ids-consistent "Different transacted ids"

                            [{:db/id :something2}]

                            [{:db/id :something}]

                            [{:db/id {:error/type :error.type/inconsisten-ids
                                      :error/data {:actual-id   :something2
                                                   :expected-id :something}}}]))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using the above code, we now have a way of testing the equivalence of transactions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn test-transactions-equivalent [actual-transaction expected-transaction]
  (let [consistent-actual-transaction (make-ids-consistent actual-transaction expected-transaction)]
    (is (= expected-transaction consistent-actual-transaction))))


(deftest test-transactions
  ;; Passes as transactions are equivalent
  (test-transactions-equivalent  [{:db/id (d/tempid :db.part/user -1)}
                                  {:db/id (d/tempid :db.part/user -1)}]

                                 [{:db/id (d/tempid :db.part/user -2)}
                                  {:db/id (d/tempid :db.part/user -2)}])

 ;; Fails as actual transaction is inconsistent with expected tranasction
  (test-transactions-equivalent  [{:db/id (d/tempid :db.part/user -1)}
                                  {:db/id (d/tempid :db.part/user -1)}]

                                 [{:db/id (d/tempid :db.part/user -4)}
                                  {:db/id (d/tempid :db.part/user -2)}]))

