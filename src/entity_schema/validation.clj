(ns entity-schema.validation
  (:require [entity-schema.entity-schema :as es]
            [datomic.api :as d])
  (:import (java.net URI)
           (java.util UUID Date Map)
           (clojure.lang Keyword)))

(defn non-map-coll? [x]
  (and (coll? x) (not (map? x))))

(defn to-coll [x]
  (if (non-map-coll? x) x #{x}))

(def valid-types
  {
   :db.type/keyword Keyword
   :db.type/string  String
   :db.type/boolean Boolean
   :db.type/long    Long
   :db.type/bigint  BigInteger
   :db.type/float   Float
   :db.type/double  Double
   :db.type/bigdec  BigDecimal
   :db.type/instant Date
   :db.type/uuid    UUID
   :db.type/uri     URI
   :db.type/bytes   (Class/forName "[B")
   :db.type/ref     #{Map Keyword}})

(declare validate-entity)

(defn error [type msg data]
  {:error/type    type
   :error/message msg
   :error/data    data})

;Error functions
(defn incorrect-type-error [value datomic-type valid-types]
  (error :error.type/incorrect-type
         "Incorrect Value Type"
         {:value        value
          :datomic-type datomic-type
          :value-type   (class value)
          :valid-types  valid-types}))

(defn unrecognised-type-error [type type-map]
  (error :error.type/unrecognised-type
         "Type not supported"
         {:type            type
          :recognised-keys type-map}))

(defn incorrect-ident-error [schema-type ident]
  (error :error.type/incorrect-type
         ":db/ident keyword does not refer to a transacted entity"
         {:db/ident           ident
          :entity.schema/type schema-type}))

(defn not-nullable-error [type-ident field]
  (error :error.type/required-field
         "Required Field"
         {:field field
          :type  type-ident}))

(defn cardinality-error [field val]
  (error :error.type/cardinality
         "Value associated with cardinality one field should not be a collection"
         {:att field
          :val val}))

(defn error? [v]
  (and (map? v) (contains? v :error/type)))

(defn validate-type [datomic-type value]
  (if-let [valid-types (->> (get valid-types datomic-type)
                            (to-coll))]
    (if (some #(isa? (class value) %) valid-types)
      value
      (incorrect-type-error value datomic-type valid-types))
    (unrecognised-type-error datomic-type valid-types)))

(defn expand-ref [db schema-type type-checked-val]
  (if (keyword? type-checked-val)
    (if-let [e (es/pull-schema-by-id db type-checked-val)]
      (if (= (get-in e [:entity.schema/type :db/ident]) schema-type)
        e
        (incorrect-ident-error schema-type type-checked-val))
      (incorrect-ident-error schema-type type-checked-val))
    type-checked-val))

(defn validate-field [db entity-schema field entity]
  (let [{nullable?                               :field/nullable?
         {field-ident             :db/ident
          {cardinality :db/ident} :db/cardinality
          {valueType :db/ident}   :db/valueType} :field/schema} field
        {{schema-type-id :db/ident} :entity.schema/type} entity-schema]
    [field-ident
     (if (contains? entity field-ident)
       (let [val (get entity field-ident)
             validated-vals (->> (to-coll val)              ;if val is a single value put into a collection
                                 (map (fn [v]
                                        (let [type-checked-val (validate-type valueType v)]
                                          (if (or (error? type-checked-val) (not= valueType :db.type/ref))
                                            type-checked-val
                                            (let [entity (expand-ref db schema-type-id type-checked-val)]
                                              (if (error? entity)
                                                entity
                                                (->> (es/derive-schema db field entity)
                                                     (validate-entity db entity))))))))
                                 (into #{}))]
         (if (= cardinality :db.cardinality/many)
           validated-vals
           (if (non-map-coll? val)
             (cardinality-error field-ident val)
             (first validated-vals))))
       (if (not nullable?)
         (not-nullable-error schema-type-id field-ident)))]))



(defn validate-entity
  [db entity {:keys [:entity.schema/fields] :as schema}]
  (->> fields
       (map #(validate-field db schema % entity))
       (filter (comp not nil? second))
       (into {})))

(defn validate
  "returns a structurally similar version of the entity with error information"
  ([db schema-type entity]
   (->> (es/derive-schema db {:field/entity-schema-type
                              {:db/ident schema-type}} entity)
        (validate-entity db entity))))

(defn valid?
  "is the validation result valid?"
  [result]
  (cond (map? result) (and (not (error? result)) (valid? (into [] (vals result))))
        (coll? result) (every? valid? result)
        :else true))

(defn assert-valid
  ([db schema-type entity]
   (let [validation (validate db schema-type entity)]
     (assert (valid? validation)
             (str "\n" (with-out-str (clojure.pprint/pprint validation)))))))











