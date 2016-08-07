(ns entity-schema.util
  (:require [entity-schema.datomic.entity-schema :as es]))




(defn pull-schema-by-type
  ([db schema-type]
   (->> (es/find-entity-schema-ids db schema-type)
        (map #(es/pull-entity-schema db %))
        (into #{})))
  ([db schema-type entity-type]
   (->> (es/find-entity-schema-ids db schema-type entity-type)
        (map #(es/pull-entity-schema db %))
        (into #{}))))

(defn first-of-one [coll]
  (assert (= (count coll) 1) (str "There is expected to be only one item in the coll" "\n"
                                  (with-out-str (clojure.pprint/pprint coll))))
  (first coll))


(defn derive-schema-id
  "Derive the schema id from the entity using either indirect type or direct schema link"
  [db field entity]
  (assert (not (nil? db)))
  (let [{{schema-type :db/ident} :field/entity-schema-type
         {schema-id :db/ident}   :field/entity-schema} field]
    (assert (or schema-type schema-id) "Field should have an :field/entity-schema-type or :field/entity-schema
    attribute")
    (if (nil? schema-id)
      (first-of-one
        (if-let [sub-type (es/derive-sub-type-id db entity)]
          (es/find-entity-schema-ids db schema-type sub-type)
          (es/find-entity-schema-ids db schema-type)))
      schema-id)))

(defn derive-schema
  "Derive the schema from the entity using either indirect type or direct schema link"
  [db field entity]
  (assert (not (nil? db)))
  (let [{{schema-type :db/ident} :field/entity-schema-type
         {schema-id :db/ident}   :field/entity-schema} field]
    (assert (or schema-type schema-id) "Field should have an :field/entity-schema-type or :field/entity-schema
    attribute")
    (if (nil? schema-id)
      (first-of-one
        (if-let [sub-type (es/derive-sub-type-id db entity)]
          (pull-schema-by-type db schema-type sub-type)
          (pull-schema-by-type db schema-type)))
      (es/pull-entity-schema db schema-id))))


(defn recursively-pull-schema [db schema-id entity]
  (let [{:keys [:entity.schema/fields] :as schema} (es/pull-entity-schema db schema-id)]
    (let [resolved-fields (->> fields
                               (map (fn [f]
                                      (if (= :db.type/ref (get-in f [:field/schema :db/valueType :db/ident]))
                                        (let [sub-schema-id (derive-schema-id db f entity)
                                              sub-schema (recursively-pull-schema db sub-schema-id (get entity (get-in f [:field/schema :db/ident])))]
                                          (assoc f :field/entity-schema sub-schema))
                                        f)))
                               (into #{}))]
      (assoc schema :entity.schema/fields resolved-fields))))



(defn ref-field? [{{{value-type :db/ident} :db/valueType} :field/schema}]
  (= :db.type/ref value-type))

(defn cardinality-many? [{{{cardinality :db/ident} :db/cardinality} :field/schema}]
  (= cardinality :db.cardinality/many))

(defn get-value [entity {{field-ident :db/ident} :field/schema}]
  (get entity field-ident))

(defn get-command [{:keys [:command-map :default-command]}
                   entity-schema-ident]
  (get command-map entity-schema-ident default-command))


(defn natural-key-coll [{:keys [:entity.schema/natural-key] :as schema} coll]
  (->> natural-key (map :db/ident) (into coll)))