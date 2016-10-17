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
            [entity-schema.validation :as v]))


(require '[spyscope.core])


(defn get-errored-id-paths [expected-entity]
  (->> (core/flatten-for-key expected-entity :db/id)
       (filter (comp v/error? second))
       (map first)))


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
                 ;;TODO here we need to remove the errored ids
                 expected-errored-ids (into #{} (map get-errored-id-paths expected-entities))
                 expected-pairs (->> expected-entities
                                     (map (fn [ent]
                                            (reduce (fn [[[inc-ent exc-ent] ks]]
                                                      [(assoc-in inc-ent ks (get-in ent ks))
                                                       (core/dissoc-in exc-ent ks)]
                                                      [{} ent]) expected-errored-ids))))

                 expected-dissoced-entities (map first expected-pairs)

                 act-pairs (->> actual-entities
                                (map (fn [ent]
                                       (reduce (fn [[[inc-ent exc-ent] ks]]
                                                 [(assoc-in inc-ent ks (get-in ent ks))
                                                  (core/dissoc-in exc-ent ks)]
                                                 [{} ent]) expected-errored-ids))))

                 act-dissoced-entities (map first act-pairs)
                 act-missing-entities (map second act-pairs)


                 transformed-actual-entities (id-func/make-ids-consistent act-dissoced-entities
                                                                          expected-dissoced-entities)

                 merged-actual-entities (->> (map vector act-missing-entities transformed-actual-entities)
                                             (map (fn [[mis act]]
                                                    (->> (core/flatten-for-key mis :db/id)
                                                         (reduce (fn [res [ks v]]
                                                                   (assoc-in res ks v))
                                                                 act))))
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

(create-comparable-invalid-single-entity-output
  "Test"
  [{:db/ident       :test-entity/string-field,
    :db/valueType   :db.type/string,
    :db/cardinality :db.cardinality/one,
    :db/unique      :db.unique/identity}]

  {:db/ident                  :entity.schema/test,
   :entity.schema/part        :db.part/entity-schema,
   :entity.schema/fields
                              [{:field/nullable? false, :field/schema :test-entity/string-field}],
   :entity.schema/natural-key [:test-entity/string-field]}

  {:command-map     {:entity.schema/test :command/insert},
   :default-command :command/look-up}

  {}
  {:test-entity/string-field
   {:error/message "Required Field",
    :error/type    :error.type/required-field}})