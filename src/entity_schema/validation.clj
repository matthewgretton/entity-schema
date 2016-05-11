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
         "Incorrect Value Type"
         {:value         value
          :expected-type expected-type
          :actual-type   actual-type}))

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
       (incorrect-type-error value type (class value)))
     (unrecognised-type-error type datomic-java-type-map))))


(defn validate-field [db
                     {schema-ident :db/ident}
                      entity
                      {nullable?                             :field/nullable?
                       {field-entity-schema :db/ident}       :field/entity-schema
                       {field-ident                 :db/ident
                        {valueType :db/ident} :db/valueType} :field/schema}]
  [field-ident (if-let [val (get entity field-ident)]
           (let [type-checked-val (validate-type valueType val)]
             (if (or (error? type-checked-val) (not= valueType :db.type/ref))
               type-checked-val
               (->> (es/derive-schema db (get entity field-ident))
                    (validate-entity db type-checked-val))))
           (if (not nullable?)
             (not-nullable-error schema-ident field-ident)))])

(defn validate-entity
  [db
   entity
   {:keys [:entity.schema/fields] :as schema}]
  (->> fields
       (map (fn [field-schema]
              (validate-field db schema entity field-schema)))
       (filter (comp not nil? second))
       (into {})))

(defn validate
  [db entity]
  (->> (es/derive-schema db entity)
      (validate-entity db entity)))



