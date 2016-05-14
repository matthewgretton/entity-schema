(ns entity-schema.validation
  (:require [entity-schema.entity-schema :as es])
  (:import (java.net URI)
           (java.util UUID Date Map)
           (clojure.lang Keyword)))


(def datomic-java-type-map
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
   :db.type/ref     Map})

(declare validate-entity)

(defn error [type msg data]
  {:error/type    type
   :error/message msg
   :error/data    data})

;Error functions
(defn incorrect-type-error [value expected-type]
  (error :error.type/incorrect-type
         "Incorrect Value Type"
         {:value         value
          :expected-type expected-type
          :actual-type   (class value)}))

(defn unrecognised-type-error [type type-map]
  (error :error.type/unrecognised-type
         "Type not supported"
         {:type            type
          :recognised-keys type-map}))

(defn not-nullable-error [entity-ident field]
  (error :error.type/required-field
         "Required Field"
         {:field     field
          :entity-id entity-ident}))

(defn error? [v]
  (and (map? v) (contains? v :error/type)))

(defn validate-type
  ([type value]
   (if-let [java-type (get datomic-java-type-map type)]
     (if (instance? java-type value)
       value
       (incorrect-type-error value type))
     (unrecognised-type-error type datomic-java-type-map))))


(defn validate-value [db field val]
  (let [valueType (get-in field [:field/schema :db/valueType :db/ident])
        type-checked-val (validate-type valueType val)]
    (if (or (error? type-checked-val) (not= valueType :db.type/ref))
      type-checked-val
      (->> (es/derive-schema db type-checked-val field)
           (validate-entity db type-checked-val)))))


(defn validate-field [db entity-schema field entity]
  (let [{nullable?                                 :field/nullable?
         {field-ident             :db/ident
          {cardinality :db/ident} :db/cardinality} :field/schema} field
        {schema-id :db/ident} entity-schema]
    [field-ident
     (if (contains? entity field-ident)
       (let [val (get entity field-ident)]
         (if (= cardinality :db.cardinality/many)
           (if (coll? val)
             (->> val (map (partial validate-value db field)) (into #{}))
             #{(validate-value db field val)})
           (validate-value db field val)))
       (if (not nullable?)
         (not-nullable-error schema-id field-ident)))]))


(defn validate-entity
  [db
   entity
   {:keys [:entity.schema/fields] :as schema}]
  (->> fields
       (map (fn [field-schema]
              (validate-field db schema field-schema entity)))
       (filter (comp not nil? second))
       (into {})))

(defn validate
  [db schema-id entity]
  (->> (es/derive-schema db entity {:field/entity-schema {:db/ident schema-id}})
       (validate-entity db entity)))



