(ns util.datomic_id_function
  "Code to check the consistency of transaxion data output.

  It's difficult to test code that prouduces transaction data, as temporary ids are generated randomly. The code in
  this gist provides a way to test the equivalence of two transactions."
  (:require [entity-schema.util.bimap :as bimap]
            [entity-schema.util.core :as core]))


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


(defn make-transaction-item-ids-consistent
  "Make actual entity ids consistent with expected entity ids"
  ([actual-entity expected-entity]
   (make-transaction-item-ids-consistent actual-entity expected-entity (bimap/create-empty)))
  ([actual-entity expected-entity input-exp-act-bimap]
   (->> (core/flatten-for-key expected-entity :db/id)
        (reduce (fn [[exp-act-bimap output] [expected-id-path expected-id]]
                  (if-let [actual-id (get-in actual-entity expected-id-path)]
                    (cond

                      ;; actual and expected ids are consistently unmapped, and ids are already transacted
                      ;; or partitions are the same
                      (and (bimap/consistently-unmapped? exp-act-bimap expected-id actual-id)
                           (or (and (is-transacted-db-id? actual-id) (= actual-id expected-id))
                               (and (and (is-temp-db-id? actual-id) (is-temp-db-id? expected-id))
                                    (= (:part actual-id) (:part expected-id)))))
                      [(bimap/assoc exp-act-bimap expected-id actual-id) (assoc-in output expected-id-path expected-id)]

                      ;; actual and expected ids are consistently mapped
                      (bimap/consistently-mapped? exp-act-bimap expected-id actual-id)
                      [exp-act-bimap (assoc-in output expected-id-path expected-id)]

                      ;; there's an inconsistency between the actual and expected ids.
                      :else
                      (let [error (->> (error :error.type/inconsisten-ids
                                              (->> {:expected-id   expected-id
                                                    :actual-id     actual-id
                                                    :mapped-act-id (bimap/get-value exp-act-bimap expected-id)
                                                    :mapped-exp-id (bimap/get-key exp-act-bimap actual-id)}
                                                   (filter (comp not nil? second))
                                                   (into {})))

                                       (into {}))]
                        [exp-act-bimap (assoc-in output expected-id-path error)]))
                    [exp-act-bimap (assoc-in output expected-id-path (error :error.type/expected-id {:expected-id expected-id}))]))
                [input-exp-act-bimap actual-entity]))))

(defn make-ids-consistent [actual-entities expected-ids]
  "Make all actual entity ids consistent with the expected entity ids."
  (->> (map vector actual-entities expected-ids)
       (reduce (fn [[exp-act-id-bi-map output-coll] [actual expected]]
                 (let [[new-exp-act-id-bi-map output] (make-transaction-item-ids-consistent actual expected exp-act-id-bi-map)]
                   [new-exp-act-id-bi-map (conj output-coll output)])) [(bimap/create-empty) []])
       (second)))


