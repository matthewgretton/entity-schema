(ns entity-schema.validation
  (:require [entity-schema.entity-schema :as es]
            [datomic.api :as d])
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
   :db.typ/bigdec   BigDecimal
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
(defn incorrect-type-error [value expected-type actual-type]
  (error :error.type/incorrect-type
         "Type of value is not correct"
         {:value         value
          :expected-type expected-type
          :actual-type   actual-type}))

(defn unrecognised-type-error [type type-map]
  (error :error.type/unrecognised-type
         "Type is not recognised"
         {:type            type
          :recognised-keys type-map}))

(defn not-nullable-error [entity field]
  (error :error.type/nullable-field
         "Non nullable field. Is expected that the field is contained or can be derived from the row"
         {:field  field
          :entity entity}))

(defn check-type
  ([type value]

   (if-let [java-type (get datomic-java-type-map type)]
     (if (instance? java-type value)
       value
       (incorrect-type-error value type (class value)))
     (unrecognised-type-error type datomic-java-type-map))))

(defn validate-field [entity
                      {nullable?                         :field/nullable?
                       field-entity-schema               :field/entity-schema
                       {:keys [:db/valueType :db/ident]} :field/schema}]
  [ident (if-let [val (get entity ident)]
           (let [type-checked-val (check-type valueType val)]
             (if (= valueType :db.type/ref)
               (validate-entity type-checked-val field-entity-schema)
               type-checked-val))
           (if (not nullable?)
             (not-nullable-error entity ident)))])



(defn validate-entity
  ([entity
    {:keys [:entity.schema/fields]}]
   (->> fields
        (map (fn [field-schema]
               (validate-field entity field-schema)))
        (filter (comp not nil? second))
        (into {}))))

(defn validate [db {:keys [:db/ident :event/instant] :as row}]
  (let [db-asof (d/as-of db instant)
        schema (es/pull-expanded-schema db-asof ident)]
    (validate-entity row schema)))

(if-let [val (get {:b 1} :a)]
  val)
