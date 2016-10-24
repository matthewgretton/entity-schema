(ns entity-schema.test-utils
  (:require [datomic.api :as d]
            [entity-schema.datomic.datomic-helper :as dh]
            [entity-schema.datomic.schema-helper :as schema-helper]
            [entity-schema.datomic.entity-schema-data :as esd]
            [entity-schema.datomic.entity-schema :as es]
            [entity-schema.processor :as p]
            [clojure.test :refer :all]
            [util.datomic_id_function :as id-func]
            [entity-schema.util.core :as core]
            [entity-schema.validation :as v]
            [entity-schema.functions :as f]))


(require '[spyscope.core])


(defn get-errored-id-paths [expected-entity]
  (->> (core/flatten-for-key expected-entity :db/id)
       (filter (comp v/error? second))
       (map first)))



(defn errored-db-id-data [expected-entities]
  (let [per-entity-errored-ids (into [] (map get-errored-id-paths expected-entities))

        data-pairs (->> (map vector expected-entities per-entity-errored-ids)
                                (map (fn [[ent e-ids]]
                                       (reduce (fn [[inc-ent exc-ent] ks]
                                                 (if (empty? ks)
                                                   [inc-ent exc-ent]
                                                   [(assoc-in inc-ent ks (get-in ent ks))
                                                    (core/dissoc-in exc-ent ks)]
                                                   ))
                                               [{} ent] e-ids))))
        entities-no-errored-db-id-data (map second data-pairs)
        errored-db-id-data (map first data-pairs)]
    [errored-db-id-data entities-no-errored-db-id-data]))

(def conn (let [uri (dh/create-in-mem-db-uri "test")]
            (d/create-database uri)
            (d/connect uri)))

(defn create-comparable-output
  ([desc fields schema command-data input-entities [expected-pairs expected-errored?] data-transactions]
   (let [uri (dh/create-in-mem-db-uri "entity-db")
         conn (do (d/create-database uri) (d/connect uri))]
     @(d/transact conn (->> fields (map #(dh/create-field %)) (into [])))
      @(d/transact conn  data-transactions)

     (let [db (d/db conn)
           datomic-compat-schema (schema-helper/make-schema-stub-compatible-with-datomic db schema)]
       @(d/transact conn esd/all-fields)
       @(d/transact conn [datomic-compat-schema])
       (let [pulled-schema (f/recursively-pull-schema (d/db conn) (:db/ident datomic-compat-schema))
           ;;(es/pull-entity-schema (d/db conn) (:db/ident datomic-compat-schema))
           ]
         (let [[actual-pairs actual-errored?] (p/process-all-entities db pulled-schema command-data input-entities)]
           (d/delete-database uri)
           (let [actual-entities (map first actual-pairs)
                 expected-entities (map first expected-pairs)
                 actual-errors (map second actual-pairs)

                 [_ expected-entities-no-errored-db-id] (errored-db-id-data expected-entities)

                 [actuaal-errored-db-id-data actual-entities-no-errored-db-id] (errored-db-id-data actual-entities)

                 transformed-actual-entities (id-func/make-ids-consistent actual-entities-no-errored-db-id
                                                                          expected-entities-no-errored-db-id)

                 merged-actual-entities (->> (map vector actuaal-errored-db-id-data transformed-actual-entities)
                                             (map (fn [[mis act]]
                                                    (if (empty? mis)
                                                      act
                                                      (->> (core/flatten-keys mis)
                                                           (reduce (fn [res [ks v]]
                                                                     (assoc-in res ks v))
                                                                   act)))))
                                             (into []))



                 transformed-actual-pairs (->> (map vector merged-actual-entities actual-errors) (into []))]
             [[expected-pairs expected-errored?] [transformed-actual-pairs actual-errored?] desc]))))))
  ([desc fields schema command-data input-entities [expected-pairs expected-errored?]]
   (create-comparable-output desc fields schema command-data input-entities [expected-pairs expected-errored?] [])))


;;test method
(defn create-comparable-valid-single-entity-output
  ([desc fields schema command-data input-entity expected-ouput data]
   (create-comparable-output desc fields schema command-data [input-entity] [[[expected-ouput false]] false] data))

  ([desc fields schema command-data input-entity expected-ouput]
   (create-comparable-valid-single-entity-output desc fields schema command-data input-entity expected-ouput [])))

(defn create-comparable-invalid-single-entity-output
  ([desc fields schema command-data input-entity expected-ouput data]
   (create-comparable-output desc fields schema command-data [input-entity] [[[expected-ouput true]] true] data))

  ([desc fields schema command-data input-entity expected-ouput]
   (create-comparable-invalid-single-entity-output desc fields schema command-data input-entity expected-ouput [])))

