(ns entity-schema.processor
  (:require [entity-schema.entity-schema :as es]
            [datomic.api :as d]))

(defn to-coll [x]
  (if (coll? x) x #{x}))

(declare process-entity)

(defn process-value [db field val]
  (let [valueType (get-in field [:field/schema :db/valueType :db/ident])
        type-checked-val (validate-type valueType val)]
    (if (or (error? type-checked-val) (not= valueType :db.type/ref))
      type-checked-val
      (let [schema-type (get-in field [:field/entity-schema-type :db/ident])
            entity (expand-ref db schema-type type-checked-val)]
        (if (error? entity)
          entity
          (->> (es/derive-schema db field entity)
               (process-entity db entity)))))))

(defn process-field [db field entity]
  (let [{{field-ident             :db/ident
          {cardinality :db/ident} :db/cardinality} :field/schema} field]
    [field-ident
     (if (contains? entity field-ident)
       (let [val (get entity field-ident)]
         (if (= cardinality :db.cardinality/many)
           (->> (to-coll val) ;if val is a single value put into a collection
                (map (partial process-value db field))
                (into #{}))
           (process-value db field val)))
       nil)]))


(defn process-entity
  [db entity {:keys [:entity.schema/fields] :as schema}]
  (->> fields
       (map #(process-field db schema % entity))
       (filter (comp not nil? second))
       (into {})))

