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
   :db.type/ref     #{Keyword Map}})

(def java-datomic-type-map
  (clojure.set/map-invert datomic-java-type-map))

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
          :actual-type   (get java-datomic-type-map (class value))}))

(defn unrecognised-type-error [type type-map]
  (error :error.type/unrecognised-type
         "Type not supported"
         {:type            type
          :recognised-keys type-map}))

(defn not-nullable-error [type-ident field]
  (error :error.type/required-field
         "Required Field"
         {:field field
          :type  type-ident}))

(defn error? [v]
  (and (map? v) (contains? v :error/type)))

(defn to-coll [x]
  (if (coll? x) x #{x}))

(defn validate-type
  ([type value]
   (if-let [java-types (to-coll (get datomic-java-type-map type))]
     (if (some #(instance? % value) java-types)
       value
       (incorrect-type-error value type))
     (unrecognised-type-error type datomic-java-type-map))))

(defn validate-value [db field val]
  (let [valueType (get-in field [:field/schema :db/valueType :db/ident])
        schema-type (get-in field [:field/entity-schema-type :db/ident])
        {:keys [:entity.schema/sub-type] :as type-checked-val} (validate-type valueType val)]
    (if (or (error? type-checked-val) (not= valueType :db.type/ref))
      type-checked-val
      (->> (es/derive-schema db schema-type sub-type)
           (validate-entity db type-checked-val)))))



(defn validate-field [db entity-schema field entity]
  (let [{nullable?                                 :field/nullable?
         {field-ident             :db/ident
          {cardinality :db/ident} :db/cardinality} :field/schema} field
        {{type-id :db/ident} :entity.schema/type} entity-schema]
    [field-ident
     (if (contains? entity field-ident)
       (let [val (get entity field-ident)]
         (if (= cardinality :db.cardinality/many)
           (->> (to-coll val)                               ;if val is a single value put into a collection
                (map (partial validate-value db field))
                (into #{}))
           (validate-value db field val)))
       (if (not nullable?)
         (not-nullable-error type-id field-ident)))]))

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
  "returns a structurally similar version of the entity with error information"
  ([db schema-type {:keys [:entity.schema/sub-type] :as entity}]
   (->> (es/derive-schema db schema-type sub-type)
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






