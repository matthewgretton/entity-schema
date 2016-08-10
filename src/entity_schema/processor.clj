(ns entity-schema.processor
  (:require [clojure.core.reducers :as r]
            [entity-schema.datomic.entity-schema :as es]
            [entity-schema.validation :as v]
            [entity-schema.util :as u]))

(defn assoc-if-not-nil [map key value]
  (if (nil? value) map (assoc map key value)))

(defn ref-identifier-type? [value]
  "Does Is the value of an identifier to an entity? "
  (some (fn [t] (isa? (class value) t)) v/ref-identifier-types))

(defn expand-ref [db unexpanded-ref]
  (es/pull-entity-schema db unexpanded-ref))

(defn merge-id-caches
  ([id-cache-0 id-cache-1]
   (merge-with (fn [l r] (merge l r)) id-cache-0 id-cache-1))
  ([id-cache-0 id-cache-1 id-cache-2]
   (-> (merge-id-caches id-cache-0 id-cache-1)
       (merge-id-caches id-cache-2))))

(defn update-id-cache
  ([id-cache ident natural-key-value-set id]
   (merge-id-caches id-cache {ident {natural-key-value-set id}})))

(defn look-up-db-id [db schema entity]
  "let's get this to return [id in-db?]"
  (let [natural-key-list (u/natural-key-coll schema [])]
    (es/look-up-entity-id db natural-key-list entity)))


(defn get-new-id [id-cache entity {:keys [:db/ident :entity.schema/part] :as schema}]
  (assert part (str "partition should be defined in schema: " schema))
  (let [natural-key-list (u/natural-key-coll schema [])
        natural-key-value-set (->> natural-key-list (map #(get entity %)) (into #{}))]
    (if-let [id (get-in id-cache [ident natural-key-value-set])]
      [id id-cache]
      (let [new-id (es/generate-db-id (:db/ident part))
            upd-id-cache (update-id-cache id-cache ident natural-key-value-set new-id)]
        [new-id upd-id-cache]))))



(defn get-id
  "return [entity-in-db? db-id id-cache errored?]"
  ([db schema command-data entity id-cache]
   (let [db-id (look-up-db-id db schema entity)
         entity-in-db? (not (nil? db-id))
         command (u/get-command db command-data schema entity)
         [validated-db-id db-id-errored?] (v/validate-db-id command schema db-id)]
     (if db-id-errored?
       [entity-in-db? validated-db-id id-cache db-id-errored?]
       (if entity-in-db?
         [entity-in-db? validated-db-id id-cache db-id-errored?]
         (let [[new-id upd-id-cache] (get-new-id id-cache entity schema)]
           [entity-in-db? new-id upd-id-cache db-id-errored?]))))))



(declare process-entity)

(defn process-ref-entity [db command-data field value id-cache]
  (let [[expanded-value expand-errored?] (if (ref-identifier-type? value)
                                           (->> (expand-ref db value) (v/validate-expanded-ref field value))
                                           [value false])]
    (if expand-errored?
      [value id-cache false]
      (let [schema (u/derive-schema db field expanded-value)
            processed-entity (process-entity db schema command-data expanded-value id-cache)]
        processed-entity))))

(defn process-valid-value [db command-data field value id-cache]
  (if (u/ref-field? field)
    (process-ref-entity db command-data field value id-cache)
    [value id-cache false]))

(defn validate-single-value [entity-in-db? field value]
  (v/thread-validation-functions field value [
                                              (partial v/validate-nullibility entity-in-db?)
                                              (partial v/validate-type)
                                              (partial v/validate-function)
                                              ]))

(defn process-one-value [db entity-in-db? command-data field value id-cache]
  (let [[validated-value errored?] (validate-single-value entity-in-db? field value)]
    (if errored?
      [validated-value id-cache errored?]
      (process-valid-value db command-data field validated-value id-cache))))

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
  (let [[cardinality-validated-value cardinality-errored?] (v/validate-cardinality field value)]
    (if cardinality-errored?
      [cardinality-validated-value id-cache cardinality-errored?]
      (if (u/cardinality-many? field)
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
            (let [value (u/get-value entity field)
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
   (let [natural-key-set (u/natural-key-coll schema #{})
         [key-fields non-key-fields] (split-fields-by-natural-key fields natural-key-set)
         [key-field-entity key-field-id-cache key-fields-errored?] (process-fields db false command-data entity id-cache key-fields)]

     (if key-fields-errored?
       [key-field-entity key-field-id-cache key-fields-errored?]
       (let [[entity-in-db? db-id upd-key-field-id-cache db-id-error?] (get-id db schema command-data key-field-entity key-field-id-cache)
             upd-key-field-entity (assoc key-field-entity :db/id db-id)]

         (if db-id-error?
           [upd-key-field-entity upd-key-field-id-cache db-id-error?]

           (let [[non-key-field-entity non-key-id-cache non-key-fields-errored?] (process-fields db
                                                                                                 entity-in-db?
                                                                                                 command-data
                                                                                                 entity upd-key-field-id-cache non-key-fields)
                 merged-entity (merge upd-key-field-entity non-key-field-entity)]
             [merged-entity non-key-id-cache non-key-fields-errored?]))))))

  ([db schema-type command-data entity]
   (let [schema (u/derive-schema db {:field/entity-schema-type {:db/ident schema-type}} entity)]
     (let [[entity _ errored?] (process-entity db schema command-data entity {})]
       [entity errored?]))))


;;How do I want this to work.




; entity -> entity.schema
; (entity, schema) -> s

; So what when we have a supertype and a sub type - Do we every need to derive?

; should





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
                         (intersect-with (fn [l _] (es/generate-db-id (:part l))) l-map r-map)) l-id-cache r-id-cache)
       (filter (comp not empty? second))
       (into {})))

(defn combine-result
  "Combine the results from two parrallel runs"
  ([] [[] {} false])
  ([[l-es l-id-cache l-errored?] [r-es r-id-cache r-errored?]]
   (let [cache-intersect (intersect-id-cache l-id-cache r-id-cache)
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

;;TODO also need a way of just using the entity id
(defn process-all-entities-from-type
  "Returns [[entity errored?]... any-errored?] "
  ([db schema-type command-data entities]
   (let [[es _ errored?]
         (r/fold combine-result
                 (fn [[es id-cache current-errored?] entity]
                   (let [schema (u/derive-schema db {:field/entity-schema-type {:db/ident schema-type}} entity)
                         [processed-entity updated-id-cache errored?] (process-entity db schema command-data entity id-cache)]
                     [(conj es [processed-entity errored?]) updated-id-cache (or current-errored? errored?)]))
                 entities)]
     [es errored?])))



(defn process-all-entities
  "Returns [[entity errored?]... any-errored?]

  Note that entities must be reduceable to take advantage of multiple cores so a lazy seq will not take advantage"
  ([db schema-id command-data entities]
   (let [[es _ errored?]
         (r/fold combine-result
                 (fn [[es id-cache current-errored?] entity]
                   (let [schema (es/pull-entity-schema db schema-id)
                         [processed-entity updated-id-cache errored?] (process-entity db schema command-data entity id-cache)]
                     [(conj es [processed-entity errored?]) updated-id-cache (or current-errored? errored?)]))
                 entities)]
     [es errored?])))

(defn get-entities-from-process-result [[entity-error-pairs errored?]]
  (assert (not errored?))
  (map first entity-error-pairs))

(defn get-errors-from-process-result [[list errored?]]
  (if errored?
    (->> (into [] list)
         (filter (comp second)))
    []))


















