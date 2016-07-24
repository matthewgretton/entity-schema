(ns entity-schema.processor
  (:require [clojure.core.reducers :as r]
            [entity-schema.datomic.entity-schema :as esp]
            [entity-schema.datomic.entity-schema-util :as es])
  (:import (java.net URI)
           (java.util UUID Date Map)
           (clojure.lang Keyword)))

;; general util methods
(defn coll-not-map? [x]
  (and (coll? x) (not (map? x))))

(defn to-coll-not-map [x]
  (if (coll-not-map? x) x #{x}))

(defn assoc-if-not-nil [map key value]
  (if (nil? value) map (assoc map key value)))

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
  (error :error.type/incorrect-ident-error
         ":db/ident keyword does not refer to a transacted entity"
         {:db/ident ident}))

(defn not-nullable-error []
  (error :error.type/required-field
         "Required Field"
         {}))

(defn incompatible-cardinality-error [value]
  (error :error.type/cardinality
         "Value associated with cardinality one field should not be a collection"
         {:value value}))

(defn unrecognised-cardinality-error [card]
  (error :error.type/un-recognised-cardinality
         "Do not recognise cardinality"
         {:cardinality card}))

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

;; entity schema helper functions
(defn ref-field? [{{{value-type :db/ident} :db/valueType} :field/schema}]
  (= :db.type/ref value-type))

(defn cardinality-many? [{{{cardinality :db/ident} :db/cardinality} :field/schema}]
  (= cardinality :db.cardinality/many))

(defn get-value [entity {{field-ident :db/ident} :field/schema}]
  (get entity field-ident))


;;Validating functions - This are functions [value field] -> [validated-value errored?]
(defn validate-type [{{{value-type :db/ident} :db/valueType} :field/schema} value]
  (if-let [valid-types (->> (get valid-types value-type)
                            (to-coll-not-map))]
    (if (some #(isa? (class value) %) valid-types)
      [value false]
      [(incorrect-type-error value value-type valid-types) false])
    [(unrecognised-type-error value-type valid-types) false]))

(defn validate-nullibility [entity-in-db? {nullable? :field/nullable?} value]
  (if (not (nil? value))
    [value false]
    (if (not (or entity-in-db? nullable?))
      [(not-nullable-error) true]
      [nil false])))

(defn validate-cardinality [field value]
  (let [cardinality (get-in field [:field/schema :db/cardinality :db/ident])]
    (cond (= cardinality :db.cardinality/many) [value false]
        (= cardinality :db.cardinality/one) (if (coll-not-map? value)
                                              [(incompatible-cardinality-error value) true]
                                              [value false])
        :else [(unrecognised-cardinality-error cardinality) true])))

(defn type-to-expand? [value]
  (or (keyword? value)
      ;(integer? value) ;Need to add in logic for this.
      ))

(defn expand-ref [db {{schema-type-id :db/ident} :field/entity-schema-type} value]
  (if (type-to-expand? value)
    (if-let [e (esp/pull-entity-schema db value)]
      (if (= (get-in e [:entity.schema/type :db/ident]) schema-type-id)
        [e false]
        [(incorrect-ident-error value) true])
      [(incorrect-ident-error value) true])
    [value false]))

(defn merge-id-caches
  ([id-cache-0 id-cache-1]
   (merge-with (fn [l r] (merge l r)) id-cache-0 id-cache-1))
  ([id-cache-0 id-cache-1 id-cache-2]
   (-> (merge-id-caches id-cache-0 id-cache-1)
       (merge-id-caches id-cache-2))))

(defn update-id-cache
  ([id-cache ident natural-key-value-set id]
   (merge-id-caches id-cache {ident {natural-key-value-set id}})))

(defn get-command [{:keys [:command-map :default-command]}
                   entity-schema-ident]
  (get command-map entity-schema-ident default-command))

(defn natural-key-coll [{:keys [:entity.schema/natural-key]} coll]
  (->> natural-key (map :db/ident) (into coll)))

;; TODO what happens if entity passed in is in error state???


(defn enrich-entity-with-id
  "Associate a :db/id element with the entity according to the specified command. Updates the id-cache to reflect
  any new ids created.

  Returns [entity-in-db? entity-with-id updated-id-cache valid?]"
  ([db {:keys [:db/ident :entity.schema/part] :as schema} command-data entity id-cache]
   (let [command (get-command command-data ident)
         natural-key-list (natural-key-coll schema [])
         natural-key-value-set (->> natural-key-list (map #(get entity %)) (into #{}))]
     (if-let [id (esp/look-up-entity-id db natural-key-list entity)]
       ;;entity already exists
       (let [[entity-with-id errored?] (cond
                                         (= command :command/insert)
                                         [(assoc entity :db/id (entity-not-expected-to-exist-error natural-key-list entity)) true]
                                         (contains? #{:command/update :command/upsert :command/look-up} command)
                                         [(assoc entity :db/id id) false])]
         [true entity-with-id id-cache errored?])
       ;; entity does not exist
       (let [[entity-with-id updated-id-cache]
             (cond
               (contains? #{:command/insert :command/upsert} command)
               (if-let [id (get-in id-cache [ident natural-key-value-set])]
                 [(assoc entity :db/id id) id-cache false]
                 (let [new-id (esp/generate-db-id db (:db/ident part))
                       upd-id-cache (update-id-cache id-cache ident natural-key-value-set new-id)]
                   [(assoc entity :db/id new-id) upd-id-cache false]))

               (contains? #{:command/look-up :command/update} command)
               [(assoc entity :db/id (entity-expected-to-exist-error natural-key-list entity)) id-cache true])]
         [false entity-with-id updated-id-cache])))))

(declare process-entity)

(defn process-ref-entity [db command-data field id-cache value]
  (let [[expanded-value expand-errored?] (expand-ref db field value)]
    (if expand-errored?
      [value id-cache false]
      (let [schema (es/derive-schema db field expanded-value)
            processed-entity (process-entity db schema command-data expanded-value id-cache)]
        processed-entity))))

(defn process-valid-value [db command-data field id-cache value]
  (if (ref-field? field)
    (process-ref-entity db command-data field id-cache value)
    [value id-cache false]))

(defn validate-single-value [entity-in-db? field value]
  (let [[nullibility-validated-value nullibility-errored?] (validate-nullibility entity-in-db? field value)]
    (if (or (nil? nullibility-validated-value) nullibility-errored?)
      [nullibility-validated-value nullibility-errored?]
      (let [[type-validated-value type-errored?] (validate-type field nullibility-validated-value)]
        [type-validated-value type-errored?]))))


(defn process-one-value [db entity-in-db? command-data field value id-cache]
  (let [[validated-value errored?] (validate-single-value entity-in-db? field value)]
    (if errored?
      [validated-value id-cache errored?]
      (process-valid-value db command-data field id-cache validated-value))))

(defn process-many-values [db entity-in-db? command-data field many-values id-cache]
  (reduce
    (fn [[vs current-id-cache current-errored?] v]
      (let [[new-v upd-id-cache errored?] (process-one-value db entity-in-db? command-data field v current-id-cache)]
        [(conj vs new-v) upd-id-cache (or current-errored? errored?)]))
    [#{} id-cache false]
    many-values))



(defn process-value [db entity-in-db? command-data field value id-cache]
  "Get id and value for the field from the entity
  returns [id value id-cache errored?]"
  (let [[cardinality-validated-value cardinality-errored?] (validate-cardinality field value)]
    (if cardinality-errored?
      [cardinality-validated-value id-cache cardinality-errored?]
      (if (cardinality-many? field)
        (process-many-values db entity-in-db? command-data field cardinality-validated-value id-cache)
        (process-one-value db entity-in-db? command-data field cardinality-validated-value id-cache)))))


(defn split-fields-by-natural-key [fields natural-key-set]
  (let [grouped-fields (->> fields
                            (group-by (fn [{{field-id :db/ident} :field/schema}]
                                        (contains? natural-key-set field-id))))
        key-fields (get grouped-fields true [])
        non-key-fields (get grouped-fields false [])]
    [key-fields non-key-fields]))



(defn process-fields [db entity-in-db? command-data entity id-cache fields]
  (reduce (fn [[current-entity current-id-cache current-errored?] field]
            (let [value (get-value entity field)
                  [proccessed-value updated-id-cache errored?] (process-value db entity-in-db? command-data field value current-id-cache)
                  updated-entity (assoc-if-not-nil current-entity (get-in field [:field/schema :db/ident]) proccessed-value)]
              [updated-entity updated-id-cache (or errored? current-errored?)]))
          [{} id-cache false]
          fields))

(defn process-entity
  "When processing the entity the following is returned
  [entity id-cache errored?]"
  ([db {:keys [:entity.schema/fields] :as schema} command-data entity id-cache]
   (assert (not (nil? fields)))
   (let [natural-key-set (natural-key-coll schema #{})
         [key-fields non-key-fields] (split-fields-by-natural-key fields natural-key-set)
         [key-field-entity key-field-id-cache key-fields-errored?] (process-fields db false command-data entity id-cache key-fields)]

     (if key-fields-errored?
       [key-field-entity key-field-id-cache key-fields-errored?]
       (let [[entity-in-db? upd-key-field-entity upd-key-field-id-cache enrich-error?] (enrich-entity-with-id db schema command-data key-field-entity key-field-id-cache)]
         (if enrich-error?
           [upd-key-field-entity upd-key-field-id-cache enrich-error?]

           (let [[non-key-field-entity non-key-id-cache non-key-fields-errored?] (process-fields db
                                                                                                 entity-in-db?
                                                                                                 command-data
                                                                                                 entity upd-key-field-id-cache non-key-fields)
                 merged-entity (merge upd-key-field-entity non-key-field-entity)]
             [merged-entity non-key-id-cache non-key-fields-errored?]))))))

  ([db schema-type command-data entity]
   (let [schema (es/derive-schema db {:field/entity-schema-type {:db/ident schema-type}} entity)]
     (let [[entity _ errored?] (process-entity db schema command-data entity {})]
       [entity errored?]))))



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

(defn intersect-id-cache [db l-id-cache r-id-cache]
  (->> (intersect-with (fn [l-map r-map]
                         (intersect-with (fn [l _] (esp/generate-db-id db (:part l))) l-map r-map)) l-id-cache r-id-cache)
       (filter (comp not empty? second))
       (into {})))

(defn combine-result
  "Combine the results from two parrallel runs"
  ([] [[] {} false])
  ([db] (fn
          ([] (combine-result))
          ([[l-es l-id-cache l-errored?] [r-es r-id-cache r-errored?]]
           (combine-result db [l-es l-id-cache l-errored?] [r-es r-id-cache r-errored?]))))
  ([db [l-es l-id-cache l-errored?] [r-es r-id-cache r-errored?]]
   (let [cache-intersect (intersect-id-cache db l-id-cache r-id-cache)
         merged (merge-id-caches l-id-cache r-id-cache cache-intersect)
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
     [(r/cat transformed-l-es transformed-r-es) merged (or l-errored? r-errored?)])))

(defn process-all-entities
  "Returns [[entity errored?]... any-errored?] "
  ([db schema-type command-data entities]
   (let [[es _ errored?]
         (r/fold 1 (combine-result db)
                 (fn [[es id-cache current-errored?] entity]
                   (let [schema (es/derive-schema db {:field/entity-schema-type {:db/ident schema-type}} entity)
                         [processed-entity updated-id-cache errored?] (process-entity db schema command-data entity id-cache)]
                     [(conj es [processed-entity errored?]) updated-id-cache (or current-errored? errored?)]))
                 entities)]
     [es errored?])))

(defn get-entities-from-process-result [[entity-error-pairs errored?]]
  (assert (not errored?))
  (map first entity-error-pairs))
















