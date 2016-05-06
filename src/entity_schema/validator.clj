(ns entity-schema.validator
  (:require [entity-schema.entity-schema :as es]
            [datomic.api :as d])
  (:import (java.net URI)
           (java.util UUID Date)
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
   :db.type/ref     nil})


(declare validate-row)


(defn error [type msg data]
  {:error/type    type
   :error/message msg
   :error/data data})

;Error functions
(defn incorrect-type-error [value expected-type actual-type]
  (error :error.type/incorrect-type
         "Type of value is not correct"
         {:value value
          :expected-type expected-type
          :actual-type actual-type}))

(defn unrecognised-type-error [type type-map]
  (error :error.type/unrecognised-type
         "Type is not recognised"
         {:type type
          :recognised-keys type-map}))

(defn not-nullable-error [row ident]
  (error :error.type/nullable-field
         "Non nullable field. Is expected that the field is contained or can be derived from the row"
         {:field ident
          :row row}))



(defn check-type
  ([value type]
   (if-let [java-type (get datomic-java-type-map type)]
     (if (instance? java-type value)
       value
       (incorrect-type-error value type (class value)))
     (unrecognised-type-error type datomic-java-type-map))))


(defn conj-into-vec [coll item]
  (into [] (conj coll item)))

(defn get-from-row [row parent-idents ident]
  (if-let [value (get row ident)]
    value
    (get row (conj-into-vec parent-idents ident))))


(defn process-field-schema [row parent-idents {:keys [:db/ident :db/valueType]}]
  (if-let [value (get-from-row row parent-idents ident)]
    ;;TODO move check type up one level
    (check-type value valueType)
    nil))

(defn validate-field [row
                      parent-idents
                      {nullable?                                          :field/nullable?
                       entity-schema                                      :field/entity-schema
                       {:keys [:db/valueType :db/ident] :as field-schema} :field/schema}]
  [ident
   (if-let [value (if (= valueType :db.type/ref)
                    (validate-row row (conj-into-vec parent-idents ident) entity-schema)
                    (process-field-schema row parent-idents field-schema))]
     value
     (if (not nullable?)
       (not-nullable-error row ident)))])

(defn validate-row
  ([row
    parent-idents
    {:keys [:entity.schema/fields]}]
   (->> fields
        (map (fn [field-schema]
               (validate-field row parent-idents field-schema)))
        (filter (comp not nil? second))
        (into {})))
  ([row entity-schema]
    (validate-row row [] entity-schema)))

(defn validate-structure [db {:keys [:db/ident :event/instant] :as row}]
  (let [db-asof (d/as-of db instant)
        schema (es/pull-expanded-schema db-asof ident)]
    (validate-row row schema)))




