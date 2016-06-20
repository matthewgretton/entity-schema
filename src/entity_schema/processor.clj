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

(defn nullable-field? [command {:keys [:entity.schema/natural-key]}
                       {{field-ident :db/ident} :field/schema
                        nullable?               :field/nullable?}]
  (cond (contains? #{:command/look-up :command/update} command)
        (or nullable? (contains? (->> natural-key
                                      :db/ident
                                      (into #{})) field-ident))
        :else nullable?))

(defn validate-field [db entity-schema field command-map default-command entity run-state]
  (let [{{field-ident             :db/ident
          {cardinality :db/ident} :db/cardinality
          {value-type :db/ident}  :db/valueType} :field/schema} field
        {{schema-type-id :db/ident} :entity.schema/type} entity-schema
        [validated-value new-run-state]
        (if (contains? entity field-ident)
          (let [value (get entity field-ident)
                [validated-values updated-run-state]
                (->> (to-coll-not-map value)
                     (reduce
                       (fn [[validated-entities current-run-state] value]
                         (let [[validated-entity updated-run-state]
                               (let [type-checked-value (validate-type value-type value)]
                                 (if (or (error? type-checked-value) (not= value-type :db.type/ref))
                                   [type-checked-value current-run-state]
                                   (let [entity (expand-ref db schema-type-id type-checked-value)]
                                     (if (error? entity)
                                       [entity current-run-state]
                                       (let [schema (es/derive-schema db field entity)
                                             [validated-entity updated-run-state]
                                             (validate-entity db schema command-map default-command entity current-run-state)]
                                         [validated-entity updated-run-state])))))]
                           [(conj validated-entities validated-entity) updated-run-state]))
                       [#{} run-state]))]
            (if (= cardinality :db.cardinality/many)
              [validated-values updated-run-state]
              (if (coll-not-map? value)
                [(incompatible-cardinality-error field-ident value) run-state]
                [(first validated-values) run-state])))
          (if (not
                (-> (get command-map field-ident default-command)
                    (nullable-field? entity-schema field)))
            [(not-nullable-error schema-type-id field-ident) run-state]))]
    [field-ident validated-value new-run-state]))

(defn update-run-state
  [run-state schema-ident natural-key id]
  (merge-with
    (partial merge-with (fn [l _] l))
    run-state {schema-ident {natural-key id}}))

(defn apply-command
  ([db {:keys [:db/ident
               :entity.schema/natural-key
               :entity.schema/part]} command validated-entity run-state]
   (assert-result-valid validated-entity)
   (let [natural-key-list (->> natural-key (map :db/ident) (into []))
         natural-key-vals (->> natural-key-list
                               (map #(get validated-entity %))
                               (into #{}))]
     (if (= command :command/validate)
       [validated-entity run-state]
       (if-let [id (dh/look-up-entity-by-natural-key db natural-key-list validated-entity)]
         ;;entity already exists
         [(cond (= command :command/insert) (entity-not-expected-to-exist-error natural-key-list validated-entity)
                (contains? #{:command/update :command/upsert} command)
                (assoc validated-entity :db/id id)
                (= command :command/look-up) {:db/ident id}) run-state]
         ;; entity does not exist
         (cond (contains? #{:command/insert :command/upsert} command)
               (if-let [id (get-in run-state [ident natural-key-vals])]
                 [(assoc validated-entity :db/id id) run-state]
                 (let [new-id (d/tempid (:db/ident part))]
                   [(assoc validated-entity :db/id new-id) (update-run-state run-state ident natural-key-vals new-id)]))
               (contains? #{:command/look-up :command/update} command)
               [(entity-expected-to-exist-error natural-key-list validated-entity) run-state]))))))

(defn split-fields [fields natural-key-set]
  (let [{key-fields true non-key-fields false}
        (->> fields
             (group-by (fn [{{field-id :db/ident} :field/schema}]
                         (contains? natural-key-set field-id))))]
    [key-fields non-key-fields]))


(defn get-key-colls [{:keys [:entity.schema/natural-key]}]
  (let [natural-key-list (->> natural-key (map :db/ident) (into []))
        natural-key-set (->> natural-key-list (into #{}))]
    [natural-key-list natural-key-set]))


(defn validate-entity
  [db {:keys [:db/ident
              :entity.schema/fields] :as schema} command-map default-command entity run-state]
  (assert (not (nil? fields)))
  (let [[_ natural-key-set] (get-key-colls schema)
        [key-fields non-key-fields] (split-fields fields natural-key-set)
        [key-field-entity new-run-state]
        (reduce (fn [[current-entity current-run-state] field]
                  (let [[field-id value updated-run-state]
                        (validate-field db schema field command-map default-command entity current-run-state)
                        updated-entity (if (nil? value)
                                         current-entity
                                         (assoc current-entity field-id value))]
                    [updated-entity updated-run-state]))
                [{} run-state]
                fields)
        command (get command-map ident default-command)
        [upd-entity upd-run-state] (apply-command db schema command key-field-entity new-run-state)
        ]
    [upd-entity upd-run-state]
    ))

(defn process
  "returns a structurally similar version of the entity with error information"
  ([db schema-type command-map default-command entity]
   (let [schema (es/derive-schema db {:field/entity-schema-type {:db/ident schema-type}} entity)]
     (first (validate-entity db schema command-map default-command entity {})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Process All ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn replace-id [map es]
  (clojure.walk/postwalk (fn [x]
                           (if (and (coll? x) (= (first x) :db/id) (contains? map (second x)))
                             [:db/id (get map (second x))]
                             x)) es))

(defn key-set [m]
  (into #{} (keys m)))

(defn intersecting-keys [l r]
  (clojure.set/intersection (key-set l) (key-set r)))

(defn intersect-with [f l r]
  (->> (intersecting-keys l r)
       (map (fn [k] [k (f (get l k) (get r k))]))
       (into {})))

(defn intersect-cache [l-cache r-cache]
  (->> (intersect-with (fn [l-map r-map]
                         (intersect-with (fn [l _] (d/tempid (:part l))) l-map r-map)) l-cache r-cache)
       (filter (comp not empty? second))
       (into {})))


(defn merge-cache [& maps]
  (apply merge-with (cons (fn [l r] (merge l r)) maps)))


(defn combine-result
  "Combine the results from two parrallel runs"
  ([] [[] {}])
  ([[l-es l-cache] [r-es r-cache]]
   (let [cache-intersect (intersect-cache l-cache r-cache)
         merged (merge-cache l-cache r-cache cache-intersect)
         [l-id-map r-id-map] (->> cache-intersect
                                  (mapcat (fn [[e m]]
                                            (->> m
                                                 (map (fn [[k new-id]]
                                                        (let [[l-id r-id] (->> [l-cache r-cache]
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
                                                              (replace-id map)))))
         ]
     [(r/cat transformed-l-es transformed-r-es) merged])))

(defn process-all [db schema-type command-map default-command entities]
  (->> entities
       (r/fold combine-result
               (fn [[es run-state] entity]
                 (let [schema (es/derive-schema db {:field/entity-schema-type {:db/ident schema-type}} entity)
                       [processed-entity updated-run-state] (validate-entity db schema command-map default-command entity run-state)]
                   [(conj es processed-entity) updated-run-state]
                   )))
       (first)))






