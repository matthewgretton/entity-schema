(ns entity-schema.validation
  (:require [entity-schema.util :as u])
  (:import (java.util Map UUID Date)
           (java.net URI)
           (clojure.lang Keyword)))

(defn coll-not-map? [x]
  (and (coll? x) (not (map? x))))

(defn to-coll-not-map [x]
  (if (coll-not-map? x) x #{x}))

(defn error? [v]
  (and (map? v) (contains? v :error/type)))

(defn error [type msg data]
  {:error/type    type
   :error/message msg
   :error/data    data})

(defn thread-validation-functions [field value validation-functions]
  "Thread value through validation functions. Short circuits if value is errored or nil"
  (if (empty? validation-functions)
    [value false]
    (let [[validated-value errored?] ((first validation-functions) field value)]
      (if (or (nil? validated-value) errored?)
        [validated-value errored?]
        (recur field validated-value (rest validation-functions))))))

(def ref-identifier-types #{Keyword Long})

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
   :db.type/ref     (conj ref-identifier-types Map)})

;;Validating functions - This are functions [value field] -> [validated-value errored?]
(defn validate-type [{{{value-type :db/ident} :db/valueType} :field/schema} value]
  (letfn [(incorrect-type-error [value datomic-type valid-types]
            (error :error.type/incorrect-type
                   "Incorrect Value Type"
                   {:value        value
                    :datomic-type datomic-type
                    :value-type   (class value)
                    :valid-types  valid-types}))

          (unrecognised-type-error [type type-map]
            (error :error.type/unrecognised-type
                   "Type not supported"
                   {:type            type
                    :recognised-keys type-map}))

          ]
    (if-let [valid-types (->> (get valid-types value-type)
                              (to-coll-not-map))]
      (if (some #(isa? (class value) %) valid-types)
        [value false]
        [(incorrect-type-error value value-type valid-types) false])
      [(unrecognised-type-error value-type valid-types) false])))

(defn validate-nullibility [entity-in-db? {nullable? :field/nullable?} value]
  (letfn [(not-nullable-error []
            (error :error.type/required-field
                   "Required Field"
                   {}))]
    (if (not (nil? value))
      [value false]
      (if (not (or entity-in-db? nullable?))
        [(not-nullable-error) true]
        [nil false]))))

(defn validate-cardinality [field value]
  (letfn [(incompatible-cardinality-error [value]
            (error :error.type/cardinality
                   "Value associated with cardinality one field should not be a collection"
                   {:value value}))

          (unrecognised-cardinality-error [card]
            (error :error.type/un-recognised-cardinality
                   "Do not recognise cardinality"
                   {:cardinality card}))]
    (let [cardinality (get-in field [:field/schema :db/cardinality :db/ident])]
      (cond (= cardinality :db.cardinality/many) [value false]
            (= cardinality :db.cardinality/one) (if (coll-not-map? value)
                                                  [(incompatible-cardinality-error value) true]
                                                  [value false])
            :else [(unrecognised-cardinality-error cardinality) true]))))

(defn validate-function [field value]
  (if-let [f (get-in field [:field/validation-function :db/fn])]
    (if-let [errored? (f value)]
      [(error :error.type/validation-function "" {:func f}) errored?]
      [value false])
    [value false]))

(defn validate-expanded-ref [field unexpanded-ref expanded-ref]
  (letfn [(incorrect-ident-error [ident]
            (error :error.type/incorrect-ident-error
                   ":db/ident keyword does not refer to a transacted entity"
                   {:db/ident ident}))]
    (if (nil? expanded-ref)
      [(incorrect-ident-error unexpanded-ref) true]
      (if (= (get-in expanded-ref [:entity.schema/type :db/ident]) (get-in field [:field/entity-schema-type :db/ident]))
        [expanded-ref false]
        [(incorrect-ident-error unexpanded-ref) true]))))

(defn validate-db-id [command schema db-id]
  (letfn [(entity-expected-to-exist-error [natural-key-list]
            (error :error.type/entity-expected-to-exist
                   "Entity expected to exist"
                   {:natural-key-list natural-key-list}))

          (entity-not-expected-to-exist-error [natural-key-list]
            (error :error.type/entity-not-expected-to-exist
                   "Entity not expected to exist"
                   {:natural-key-list natural-key-list}))]
    (if (nil? db-id)
      ;; id not in db
      (cond
        (contains? #{:command/insert :command/upsert} command)
        [db-id false]
        (contains? #{:command/look-up :command/update} command)
        [(entity-expected-to-exist-error (u/natural-key-coll schema [])) true])
      ;; id in db
      (cond
        (contains? #{:command/insert} command)
        [(entity-not-expected-to-exist-error (u/natural-key-coll schema [])) true]
        (contains? #{:command/look-up :command/update :command/upsert} command)
        [db-id false]))))



