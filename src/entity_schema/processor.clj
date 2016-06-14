(ns entity-schema.processor
  (:require [entity-schema.entity-schema :as es]
            [datomic.api :as d]
            [entity-schema.datomic-helper :as dh])
  (:import (java.net URI)
           (java.util UUID Date Map)
           (clojure.lang Keyword)))

(defn coll-not-map? [x]
  (and (coll? x) (not (map? x))))

(defn to-coll-not-map [x]
  (if (coll-not-map? x) x #{x}))

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



(defn error [type msg data]
  {:error/type    type
   :error/message msg
   :error/data    data})

(defn error? [v]
  (and (map? v) (contains? v :error/type)))

(defn valid?
  "is the validation result valid?"
  [result]
  (cond (map? result) (and (not (error? result)) (valid? (into [] (vals result))))
        (coll? result) (every? valid? result)
        :else true))

(defn assert-result-valid
  ([validation]
   (assert (valid? validation)
           (str "\n" (with-out-str (clojure.pprint/pprint validation))))))

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

(defn incompatible-cardinality-error [field val]
  (error :error.type/cardinality
         "Value associated with cardinality one field should not be a collection"
         {:att field
          :val val}))

(defn entity-expected-to-exist-error [natural-key-list entity]
  (error :error.type/entity-expected-to-exist
         "Entity expected to exist"
         {:natural-key-list natural-key-list
          :entity           entity}))

(defn entity-not-expected-to-exist-error [natural-key-list entity]
  (error :error.type/entity-not-expected-to-exist
         "Entity not expected to exist"
         {:natural-key-list natural-key-list
          :entity           entity}))


;;Validation code
(defn validate-type [datomic-type value]
  (if-let [valid-types (->> (get valid-types datomic-type)
                            (to-coll-not-map))]
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

(declare validate-entity)

(defn validate-field [db entity-schema field entity res f]
  (let [{nullable?                               :field/nullable?
         {field-ident             :db/ident
          {cardinality :db/ident} :db/cardinality
          {valueType :db/ident}   :db/valueType} :field/schema} field
        {{schema-type-id :db/ident} :entity.schema/type} entity-schema
        [validated-value new-res] (if (contains? entity field-ident)
                                    (let [val (get entity field-ident)
                                          [validated-vals new-res]
                                          (->> (to-coll-not-map val) ;if val is a single value put into a collection
                                               (reduce (fn [[es r] v]
                                                         (let [type-checked-val (validate-type valueType v)]
                                                           (if (or (error? type-checked-val) (not= valueType :db.type/ref))
                                                             [(conj es type-checked-val) r]
                                                             (let [entity (expand-ref db schema-type-id type-checked-val)]
                                                               (if (error? entity)
                                                                 [(conj es entity) r]
                                                                 (let [schema (es/derive-schema db field entity)
                                                                       [e new-r] (validate-entity db schema entity r f)]
                                                                   [(conj es e) new-r]))))))
                                                       [#{} res]))]
                                      (if (= cardinality :db.cardinality/many)
                                        [validated-vals new-res]
                                        (if (coll-not-map? val)
                                          [(incompatible-cardinality-error field-ident val) res]
                                          [(first validated-vals) res])))
                                    (if (not nullable?)
                                      [(not-nullable-error schema-type-id field-ident) res]))]
    [field-ident validated-value new-res]))



(defn validate-entity
  [db {:keys [:entity.schema/fields] :as schema} entity res xf]
  (assert (not (nil? fields)))
  (let [[ent new-r]
        (reduce (fn [[m r] field]
                  (let [[f-id v new-r] (validate-field db schema field r entity xf)
                        new-m (if (nil? v) m (assoc m f-id v))]
                    [new-m new-r]))
                [{} res]
                fields)]
    (xf db schema new-r ent)))


;;process funcs

(defn identity-process-func
  ([] nil)
  ([db schema res validated-entity]
   validated-entity))




(defn combine
  ([left schema-ident natural-key id]
   (combine left {schema-ident {natural-key id}}))
  ([left right]
   (merge-with
     (partial merge-with (fn [l _] l))
     left right)))

(defn apply-command
  ([command-map default-command

    db
    {:keys [:db/ident
            :entity.schema/natural-key
            :entity.schema/part]}
    run-state
    validated-entity
    ]
   (assert-result-valid validated-entity)
   (assert (not (nil? command-map)))
   (assert (not (nil? default-command)))
   (let [natural-key-list (->> natural-key (map :db/ident) (into []))
         command (get command-map ident default-command)
         natural-key-vals (->> natural-key-list
                               (map #(get validated-entity %))
                               (into #{})
                               )]
     (if-let [id (dh/look-up-entity-by-natural-key db natural-key-list validated-entity)]
       ;;entity already exists
       [(cond (= command :command/insert) (entity-not-expected-to-exist-error natural-key-list validated-entity)
              (= command :command/upsert) (assoc validated-entity :db/id id)
              (= command :command/look-up) (assoc validated-entity :db/id id)
              (= command :command/update) (assert false "Update not implemented yet")
              (= command :command/delete) (assert false "Delete not implemented yet")) run-state]
       ;; entity does not exist
       (cond (= command :command/insert) (if-let [id (get-in run-state [ident natural-key-vals])]
                                           [(assoc validated-entity :db/id id) run-state]
                                           (let [new-id (d/tempid (:db/ident part))]
                                             [(assoc validated-entity :db/id new-id) (combine run-state ident natural-key-vals new-id)]))
             (= command :command/upsert) (if-let [id (get-in run-state [ident natural-key-vals])]
                                           [(assoc validated-entity :db/id id) run-state]
                                           (let [new-id (d/tempid (:db/ident part))]
                                             [(assoc validated-entity :db/id new-id) (combine run-state ident natural-key-vals new-id)]))
             (= command :command/look-up) [(entity-expected-to-exist-error natural-key-list validated-entity) run-state]
             (= command :command/update) [(entity-expected-to-exist-error natural-key-list validated-entity) run-state]
             (= command :command/delete) [(entity-expected-to-exist-error natural-key-list validated-entity) run-state]))))
  )

(defn process
  "returns a structurally similar version of the entity with error information"
  ([db schema-type command-map default-command entity]
   (let [schema (es/derive-schema db {:field/entity-schema-type {:db/ident schema-type}} entity)
         process-func (partial apply-command command-map default-command)]
     (validate-entity db schema entity {} process-func))))


(defn validate
  "returns a structurally similar version of the entity with error information"
  ([db schema-type entity]
   (let [schema (es/derive-schema db {:field/entity-schema-type {:db/ident schema-type}} entity)]
     (validate-entity db schema entity nil identity-process-func))))


(defn assert-valid
  ([db schema-type entity]
   (let [validation (validate db schema-type entity)]
     (assert-result-valid validation))))


(def left
  {:entity.schema/blah1 {#{1 2} 1
                         #{1 3} 2}
   :entity.schema/blah2 {#{1 3} 1}})

(def right
  {:entity.schema/blah1 {#{1 2} 11
                         #{1 4} 22}
   :entity.schema/blah2 {#{1 4} 4}})

(def expect
  {:entity.schema/blah1 {#{1 2} 1
                         #{1 3} 2
                         #{1 4} 22}
   :entity.schema/blah2 {#{1 3} 1
                         #{1 4} 4}})



(assert (= (combine left right) expect))



