(ns entity-schema.datomic.entity-schema
  (:require [entity-schema.datomic.datomic-helper :as dh]
            [datomic.api :as d]
            [entity-schema.entity-schema :as es]))


(defn pull-schema-by-id [db entity-schema-id]
  (-> (d/pull db '[:db/ident
                   {:entity.schema/type [:db/ident]}
                   {:entity.schema/part [:db/ident]}
                   {:entity.schema/sub-type [:db/ident]}

                   {:entity.schema/fields [{:field/schema [:db/ident
                                                           {:db/cardinality [:db/ident]}
                                                           {:db/valueType [:db/ident]}]}
                                           {:field/entity-schema-type [:db/ident]}
                                           :field/nullable?]}
                   {:entity.schema/natural-key [{:list/first [:db/ident]}
                                                {:list/rest ...}]}] entity-schema-id)
      (update :entity.schema/natural-key dh/from-linked-list-entity)))

(defn get-schema-ids-by-type
  ([db schema-type entity-type]
   (->> (d/q '[:find ?e
               :in $ ?s-type ?e-type
               :where
               [?e :entity.schema/type ?s-type]
               [?e :entity.schema/sub-type ?e-type]] db schema-type entity-type)
        (map first)
        (into #{})))
  ([db schema-type]
   (->> (d/q '[:find ?e
               :in $ ?type
               :where [?e :entity.schema/type ?type]] db schema-type)
        (map first)
        (into #{}))))

(defn derive-sub-type [entity]
  (:entity.schema/sub-type entity))


(defn tempid [part]
  (d/tempid part))

;TODO add in a check that there is at least one item in the entity
(defn build-query-map [db natural-key entity]
  (assert (some (fn [k] (contains? entity k)) natural-key) (str "There needs to be at least of the natural keys in the map" "\n"

                                                                "natural-key:" "\n" (with-out-str (clojure.pprint/pprint natural-key)) "\n"

                                                                "entity:" "\n" (with-out-str (clojure.pprint/pprint entity))))
  (let [ent-sym '?e
        [where in args] (->> (map vector natural-key (range (count natural-key)))
                             (reduce
                               (fn [[wh in args] [att i]]
                                 (if-let [v (get entity att)]
                                   (let [sym (symbol (str "?" i))]
                                     [(conj wh [ent-sym att sym]) (conj in sym) (conj args v)])
                                   [(conj wh (list 'not [ent-sym att '_])) in args]))
                               [[] ['$] [db]]))]
    {:query {:find  [ent-sym]
             :in    in
             :where where}
     :args  args}))


(defn look-up-entity-by-natural-key [db natural-key entity]
  (let [query-map (build-query-map db natural-key entity)
        r (d/query query-map)]
    (if (not (empty? r))
      (do (assert (= 1 (count r)) (str "Query result should only return one result \n"
                                       "natural-key:\n" (with-out-str (clojure.pprint/pprint natural-key)) "\n"
                                       "entity:\n" (with-out-str (clojure.pprint/pprint entity)) "\n"
                                       "query-map\n" (with-out-str (clojure.pprint/pprint query-map)) "\n"
                                       "result:\n" (with-out-str (clojure.pprint/pprint r))
                                       ))
          (->> r (first) (first))))))


;Protocol - Not actually used this yet, but we'll see.

(deftype DatomicEntitySchema [db]
  es/EntitySchemaDatabase

  (->entity-schema [entity-schema-db id] (pull-schema-by-id db id))

  (->new-db-id [entity-schema-db partition] (tempid partition))

  (->sub-type-id [entity-schema-db entity] (derive-sub-type entity))

  (->entity-schema-ids [entity-schema-db schema-type] (get-schema-ids-by-type db schema-type))

  (->entity-schema-ids [entity-schema-db schema-type sub-type] (get-schema-ids-by-type db schema-type sub-type))

  (->entity-id [entity-schema-db natural-key-list entity] (look-up-entity-by-natural-key db natural-key-list entity))

  )

(defn pull-schema-by-type
  ([db schema-type]
   (->> (es/->entity-schema-ids db schema-type)
        (map #(es/->entity-schema db %))
        (into #{})))
  ([db schema-type entity-type]
   (->> (es/->entity-schema-ids db schema-type entity-type)
        (map #(es/->entity-schema db %))
        (into #{}))))

(defn first-of-one [coll]
  (assert (= (count coll) 1) (str "There is expected to be only one item in the coll" "\n"
                                  (with-out-str (clojure.pprint/pprint coll))))
  (first coll))

(defn derive-schema
  "Derive the schema from the entity"
  [db field entity]
  (assert (not (nil? db)))
  (let [{{schema-type :db/ident} :field/entity-schema-type} field]
    (assert (not (nil? schema-type)))
    (first-of-one
      (if-let [sub-type (es/->sub-type-id db entity)]
        (pull-schema-by-type db schema-type sub-type)
        (pull-schema-by-type db schema-type)))))