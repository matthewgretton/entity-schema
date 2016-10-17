(ns entity-schema.test-utils
  (:require [datomic.api :as d]
            [entity-schema.datomic.datomic-helper :as dh]
            [entity-schema.datomic.schema-helper :as schema-helper]
            [entity-schema.datomic.entity-schema-data :as esd]
            [entity-schema.datomic.entity-schema :as es]
            [entity-schema.processor :as p]
            [clojure.test :refer :all]
            [util.datomic_id_function :as id-func]))


(require '[spyscope.core])

(defn create-comparable-output
  ([desc fields schema command-data input-entities [expected-pairs expected-errored?] data-transactions]
   (let [uri (dh/create-in-mem-db-uri "entity-db")
         conn (do (d/create-database uri) (d/connect uri))]
     @(d/transact conn (->> fields (map #(dh/create-field %)) (into [])))
     @(d/transact conn data-transactions)
     (let [db (d/db conn)
           datomic-compat-schema (schema-helper/make-compatible-with-datomic db schema)]
       @(d/transact conn esd/all-fields)
       @(d/transact conn [datomic-compat-schema])
       (let [pulled-schema (es/pull-entity-schema (d/db conn) (:db/ident datomic-compat-schema))]
         (let [[actual-pairs actual-errored?] (p/process-all-entities db pulled-schema command-data input-entities)]
           (d/delete-database uri)
           (let [actual-entities (map first actual-pairs)
                 expected-entities (map first expected-pairs)
                 actual-errors (map second actual-pairs)
                 transformed-actual-entities  (id-func/make-ids-consistent actual-entities expected-entities)
                 transformed-actual-pairs (->> (map vector transformed-actual-entities actual-errors) (into []))]
             [[expected-pairs expected-errored?] [transformed-actual-pairs actual-errored?]  desc]))))))
  ([desc fields schema command-data input-entities [expected-pairs expected-errored?]]
   (create-comparable-output desc fields schema command-data input-entities [expected-pairs expected-errored?] [])))



;;test method
(defn create-comparable-valid-single-entity-output
  ([desc fields schema command-data input-entity expected-ouput data]
   (create-comparable-output desc fields schema command-data [input-entity] [[[expected-ouput false]] false]) data)

  ([desc fields schema command-data input-entity expected-ouput]
   (create-comparable-valid-single-entity-output desc fields schema command-data input-entity expected-ouput [])))

(defn create-comparable-invalid-single-entity-output
  ([desc fields schema command-data input-entity expected-ouput data]
   (create-comparable-output desc fields schema command-data [input-entity] [[[expected-ouput true]] true]) data)

  ([desc fields schema command-data input-entity expected-ouput]
   (create-comparable-invalid-single-entity-output desc fields schema command-data input-entity expected-ouput [])))

