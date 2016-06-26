(ns entity-schema.processor
  (:require [entity-schema.entity-schema :as es]
            [datomic.api :as d]
            [entity-schema.datomic-helper :as dh]
            [clojure.core.reducers :as r])
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

(defmacro error->>
  "When expr is not an errored (according to error?), threads it into the first form (via ->>),
  and when that result is not errored, through the next etc"
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (error? ~g) nil (->> ~g ~step)))
                   forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))




(defn valid?
  "is the validation result valid?"
  [result]
  (cond (map? result) (and (not (error? result)) (valid? (into [] (vals result))))
        (coll? result) (every? valid? result)
        :else true))



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

(defn get-key-set [{:keys [:entity.schema/natural-key]}]
  (->> natural-key (map :db/ident) (into #{})))

(defn validate-field [db entity-schema field entity-in-db? command-data entity id-cache]
  (let [{{field-ident             :db/ident
          {cardinality :db/ident} :db/cardinality
          {value-type :db/ident}  :db/valueType} :field/schema
         nullable?                               :field/nullable?} field
        {{schema-type-id :db/ident} :entity.schema/type} entity-schema
        [validated-value new-id-cache]
        (if (contains? entity field-ident)
          (let [value (get entity field-ident)
                [validated-values updated-id-cache]
                (->> (to-coll-not-map value)
                     (reduce
                       (fn [[validated-entities current-id-cache] value]
                         (let [[validated-entity updated-id-cache]
                               (let [type-checked-value (validate-type value-type value)]
                                 (if (or (error? type-checked-value) (not= value-type :db.type/ref))
                                   [type-checked-value current-id-cache]
                                   (let [entity (expand-ref db schema-type-id type-checked-value)]
                                     (if (error? entity)
                                       [entity current-id-cache]
                                       (let [schema (es/derive-schema db field entity)
                                             [validated-entity updated-id-cache]
                                             (validate-entity db schema command-data entity current-id-cache)]
                                         [validated-entity updated-id-cache])))))]
                           [(conj validated-entities validated-entity) updated-id-cache]))
                       [#{} id-cache]))]
            (if (= cardinality :db.cardinality/many)
              [validated-values updated-id-cache]
              (if (coll-not-map? value)
                [(incompatible-cardinality-error field-ident value) id-cache]
                [(first validated-values) id-cache])))
          ;If the entity is in the db, we assume that we don't want need to check the nullibility of the fields
          (if (not (or entity-in-db? nullable?))
            [(not-nullable-error schema-type-id field-ident) id-cache]))]
    [field-ident validated-value new-id-cache]))

(defn update-id-cache
  ([id-cache schema-ident natural-key id]
   (update-id-cache id-cache {schema-ident {natural-key id}}))
  ([id-cache id-cache-1]
   (merge-with (fn [l r] (merge l r)) id-cache id-cache-1))
  ([id-cache id-cache-1 id-cache-2]
   (-> (update-id-cache id-cache id-cache-1)
       (update-id-cache id-cache-2))))

(defn assoc-id
  "Associate a :db/id element with the validated entity according to the specified command.

  Returns [entity-in-db? validated-entity-with-id updated-id-cache]"
  ([db {:keys [:db/ident :entity.schema/natural-key :entity.schema/part] :as schema}
    {:keys [:command-map :default-command]}
    validated-entity
    id-cache]
   (let [command (get command-map ident default-command)
         natural-key-list (->> natural-key (map :db/ident) (into []))
         natural-key-set (->> natural-key-list
                              (map #(get validated-entity %))
                              (into #{}))]
     (if-let [id (dh/look-up-entity-by-natural-key db natural-key-list validated-entity)]
       ;;entity already exists
       [true (cond
               (= command :command/insert)
               (assoc validate-entity :db/id (entity-not-expected-to-exist-error natural-key-list validated-entity))

               (contains? #{:command/update :command/upsert} command)
               (assoc validated-entity :db/id id)

               (= command :command/look-up)
               {:db/id id}) id-cache]
       ;; entity does not exist
       (cons
         (cond
           (contains? #{:command/insert :command/upsert} command)
           (if-let [id (get-in id-cache [ident natural-key-set])]
             [(assoc validated-entity :db/id id) id-cache]
             (let [new-id (d/tempid (:db/ident part))
                   upd-id-cache (update-id-cache id-cache ident natural-key-set new-id)]
               [(assoc validated-entity :db/id new-id) upd-id-cache]))

           (contains? #{:command/look-up :command/update} command)
           [(assoc validate-entity :db/id (entity-expected-to-exist-error natural-key-list validated-entity)) id-cache])
         false)))))

(defn split-fields-by-natural-key [fields natural-key-set]
  (let [grouped-fields (->> fields
                            (group-by (fn [{{field-id :db/ident} :field/schema}]
                                        (contains? natural-key-set field-id))))
        key-fields (get grouped-fields true [])
        non-key-fields (get grouped-fields false [])]
    [key-fields non-key-fields]))

(defn process-fields [db schema entity-in-db? command-data entity id-cache fields]
  (reduce (fn [[current-entity current-id-cache] field]
            (let [[field-id value updated-id-cache] (validate-field db schema field entity-in-db? command-data
                                                                    entity current-id-cache)
                  updated-entity (if (nil? value)
                                   current-entity
                                   (assoc current-entity field-id value))]
              [updated-entity updated-id-cache]))
          [{} id-cache]
          fields))

(defn validate-entity
  [db {:keys [:entity.schema/fields] :as schema} command-data entity id-cache]
  (assert (not (nil? fields)))
  (let [natural-key-set (get-key-set schema)
        [key-fields non-key-fields] (split-fields-by-natural-key fields natural-key-set)
        [key-field-entity key-field-id-cache]
        (process-fields db schema false command-data entity id-cache key-fields)]
    (if (error? key-field-entity)
      [key-field-entity key-field-id-cache]
      (let [[entity-in-db? upd-key-field-entity upd-key-field-id-cache]
            (assoc-id db schema command-data key-field-entity key-field-id-cache)]
        (if (error? upd-key-field-entity)
          [upd-key-field-entity upd-key-field-id-cache]
          (let [[non-key-field-entity non-key-id-cache]
                (process-fields db schema
                                entity-in-db?
                                command-data
                                entity upd-key-field-id-cache non-key-fields)]
            [(merge upd-key-field-entity non-key-field-entity) non-key-id-cache]))))))

(defn process
  "returns a structurally similar version of the entity with error information"
  ([db schema-type command-data entity]
   (let [schema (es/derive-schema db {:field/entity-schema-type {:db/ident schema-type}} entity)]
     (first (validate-entity db schema command-data entity {})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Process All ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn replace-id [id-map es]
  (clojure.walk/postwalk (fn [x]
                           (if (and (coll? x) (= (first x) :db/id) (contains? id-map (second x)))
                             [:db/id (get id-map (second x))]
                             x)) es))

(defn key-set [m]
  (into #{} (keys m)))

(defn intersecting-keys [l r]
  (clojure.set/intersection (key-set l) (key-set r)))

(defn intersect-with [f l r]
  (->> (intersecting-keys l r)
       (map (fn [k] [k (f (get l k) (get r k))]))
       (into {})))

(defn intersect-id-cache [l-id-cache r-id-cache]
  (->> (intersect-with (fn [l-map r-map]
                         (intersect-with (fn [l _] (d/tempid (:part l))) l-map r-map)) l-id-cache r-id-cache)
       (filter (comp not empty? second))
       (into {})))

(defn combine-result
  "Combine the results from two parrallel runs"
  ([] [[] {}])
  ([[l-es l-id-cache] [r-es r-id-cache]]
   (let [cache-intersect (intersect-id-cache l-id-cache r-id-cache)
         merged (update-id-cache l-id-cache r-id-cache cache-intersect)
         [l-id-map r-id-map] (->> cache-intersect
                                  (mapcat (fn [[e m]]
                                            (->> m
                                                 (map (fn [[k new-id]]
                                                        (let [[l-id r-id] (->> [l-id-cache r-id-cache]
                                                                               (map #(get-in % [e k])))]
                                                          [l-id r-id new-id])))
                                                 (filter (fn [[a b _]]
                                                           (and (not (nil? a)) (not (nil? b))))))))
                                  (reduce
                                    (fn [[l-map r-map] [l-id r-id new-id]]
                                      [(assoc l-map l-id new-id) (assoc r-map r-id new-id)])
                                    [{} {}]))

         [transformed-l-es transformed-r-es] (->> [[l-id-map l-es] [r-id-map r-es]]
                                                  (map (fn [[map es]]
                                                         (->> (if (instance? clojure.core.reducers.Cat es)
                                                                (into [] es)
                                                                es)
                                                              (replace-id map)))))]
     [(r/cat transformed-l-es transformed-r-es) merged])))

(defn process-all
  ([db schema-type command-data entities]
   (->> entities
        (r/fold combine-result
                (fn [[es id-cache] entity]
                  (let [schema (es/derive-schema db {:field/entity-schema-type {:db/ident schema-type}} entity)
                        [processed-entity updated-id-cache] (validate-entity db schema command-data entity id-cache)]
                    [(conj es processed-entity) updated-id-cache]
                    )))
        (first))))






