(ns entity-schema.test-helper
  (:require [datomic.api :as d]
            [entity-schema.datomic.datomic-helper :as dh]
            [entity-schema.util.bimap :as bimap]
            [entity-schema.datomic.schema-helper :as schema-helper]
            [entity-schema.datomic.entity-schema-data :as esd]
            [entity-schema.datomic.entity-schema :as es]
            [entity-schema.processor :as p]
            [entity-schema.util.core :as util]
            [clojure.test :refer :all]))


(defn transform [actual expected input-exp-act-bimap]
  (->> (util/flatten-upto-key expected :db/id)
       (reduce (fn [[exp-act-bimap output] [expected-id-path expected-id]]
                 (if-let [actual-id (get-in actual expected-id-path)]
                   (cond

                     ;; consistently unmapped, but partitions are the same
                     (and (bimap/consistently-unmapped? exp-act-bimap expected-id actual-id)
                          (= (:part actual-id) (:part expected-id)))
                     [(bimap/assoc exp-act-bimap expected-id actual-id) (assoc-in output expected-id-path expected-id)]

                     ;; consistently mapped
                     (bimap/consistently-mapped? exp-act-bimap expected-id actual-id)
                     [exp-act-bimap (assoc-in output expected-id-path expected-id)]

                     ;; there's an inconsistency between the mapping and the inputs
                     :else
                     [exp-act-bimap (->> (assoc actual-id :idx {:actual-id     actual-id
                                                                :expected-id   expected-id
                                                                :mapped-act-id (bimap/get-value exp-act-bimap expected-id)
                                                                :mapped-exp-id (bimap/get-key exp-act-bimap actual-id)})
                                         (assoc-in output expected-id-path))])
                   [exp-act-bimap output])) [input-exp-act-bimap actual])))

(defn transform-all [actuals expecteds]
  (->> (map vector actuals expecteds)
       (reduce (fn [[exp-act-id-bi-map output-coll] [actual expected]]
                 (let [[new-exp-act-id-bi-map output] (transform actual expected exp-act-id-bi-map)]
                   [new-exp-act-id-bi-map (conj output-coll output)])) [(bimap/create-empty) []])
       (second)))


(require 'spyscope.core)

(defn test-entities [test-desc fields schema command-data input-entities [expected-pairs expected-errored?]]
  (let [uri (dh/create-in-mem-db-uri "entity-db")
        conn (do (d/create-database uri) (d/connect uri))]
    @(d/transact conn (->> fields (map #(dh/create-field %)) (into [])))
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
                transformed-actual-entities (transform-all actual-entities expected-entities)
                transformed-actual-pairs (->> (map vector transformed-actual-entities actual-errors) (into []))]
            (is (= [transformed-actual-pairs actual-errored?]
                   [expected-pairs expected-errored?]) test-desc)))))))

;;test method
(defn test-valid-single-entity [test-desc fields schema command-data input-entity expected-ouput]
  (test-entities test-desc fields schema command-data [input-entity] [[[expected-ouput false]] false]))

(defn test-invalid-single-entity [test-desc fields schema command-data input-entity expected-ouput]
  (test-entities test-desc fields schema command-data [input-entity] [[[expected-ouput true]] true]))

