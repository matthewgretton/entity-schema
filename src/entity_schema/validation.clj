(ns entity-schema.validation
  (:require [entity-schema.entity-schema :as es]
            [datomic.api :as d])
  (:import (java.net URI)
           (java.util UUID Date Map)
           (clojure.lang Keyword)))



(defmacro error->
  "When expr is not error?, threads it into the first form (via ->),
  and when that result is not error?, through the next etc"
  {:added "1.5"}
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (error? ~g) ~g (-> ~g ~step)))
                   forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro error->>
  "When expr is not error?, threads it into the first form (via ->>),
  and when that result is not error?, through the next etc"
  {:added "1.5"}
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (error? ~g) ~g (->> ~g ~step)))
                   forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))




(defn to-coll [x]
  (if (coll? x) x #{x}))

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


(defn incorrect-ident-error [ident]
  (error :error.type/incorrect-type
         ":db/ident keyword does not refer to a transacted entity"
         {:db/ident ident}))

(defn not-nullable-error [type-ident field]
  (error :error.type/required-field
         "Required Field"
         {:field field
          :type  type-ident}))

(defn error? [v]
  (and (map? v) (contains? v :error/type)))


(defmacro error->
  "When expr is not error?, threads it into the first form (via ->),
  and when that result is not error?, through the next etc"
  {:added "1.5"}
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (error? ~g) ~g (-> ~g ~step)))
                   forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro error->>
  "When expr is not error?, threads it into the first form (via ->>),
  and when that result is not error?, through the next etc"
  {:added "1.5"}
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (error? ~g) ~g (->> ~g ~step)))
                   forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defn validate-type [datomic-type value]
  (if-let [valid-types (->> (get valid-types datomic-type)
                            (to-coll))]
    (if (some #(isa? (class value) %) valid-types)
      value
      (incorrect-type-error value datomic-type valid-types))
    (unrecognised-type-error datomic-type valid-types)))




;(defn validate-type [])
;
;(defn transform-value [])
;
;(defn validate [])


(defn validate-value [db field val]
  (let [valueType (get-in field [:field/schema :db/valueType :db/ident])
        type-checked-val (validate-type valueType val)]
    (if (or (error? type-checked-val) (not= valueType :db.type/ref))
      type-checked-val
      (let [expanded-entity (cond (map? type-checked-val)
                                  type-checked-val

                                  (keyword? type-checked-val)
                                  (if-let [x (es/pull-schema-by-id db type-checked-val)]
                                                                x
                                                                (incorrect-ident-error type-checked-val)))]

        (if (error? expanded-entity)
          expanded-entity
          (->> (es/derive-schema db field expanded-entity)
               (validate-entity db expanded-entity)))))))

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











