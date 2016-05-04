(ns entity-schema.entity-schema
  (:require [datomic.api :as d]))


(defn- recursive-pull [db result]
  (let [enum-fields #{:db/valueType :db/cardinality}]
    (cond
      (map? result) (->> (dissoc (d/pull db '[*] (:db/id result)) :db/id)
                         (map (fn [[k v]]
                                [k (let [m (recursive-pull db v)]
                                     (if (contains? enum-fields k)
                                       (:db/ident m)
                                       m))]))
                         (into {}))

      (coll? result) (->> result
                          (map (fn [v]
                                 (recursive-pull db v)))
                          (into (empty result)))
      :else result)))

(defn pull-expanded-schema [db id]
  (recursive-pull db {:db/id id}))



(defn pull-unexpanded-schema [db id]
  (let [{:keys [:db/ident :entity.schema/fields :entity.schema/natural-key]}
        (d/pull db '[:db/ident
                     {:entity.schema/fields [{:field/schema [:db/ident]}
                                             :field/required?
                                             {:field/entity-schema [:db/ident]}]}
                     {:entity.schema/natural-key [:db/ident]}] id)]
    {
     :db/ident                  ident
     :entity.schema/fields      (->> fields
                                     (map (fn [{:keys [:field/schema
                                                       :field/required?
                                                       :field/entity-schema]}]
                                            (let [stub {:field/schema        (:db/ident schema)
                                                        :field/required?     required?}]
                                              (if entity-schema
                                                (assoc stub :field/entity-schema (:db/ident entity-schema))
                                                stub))
                                            ))
                                     (filter (comp not nil? second))
                                     (into #{}))

     :entity.schema/natural-key (->> natural-key
                                     (map :db/ident)
                                     (into #{}))}))


(defn get-schema [db {:keys [:db/ident :event/instant]}]
  (pull-expanded-schema (d/as-of db instant) ident))



(def entity-schema-fields
  [{:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :field/schema
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/one}

   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :field/required?
    :db/valueType          :db.type/boolean
    :db/cardinality        :db.cardinality/one}

   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :field/entity-schema
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/one}

   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :entity.schema/fields
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/many}

   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :entity.schema/natural-key
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/many}])


(defn bootstrap [conn]
  @(d/transact conn entity-schema-fields))



(defn modify-entity-schema-optionality-tx [db entity-id field-id required?]
  (->> (d/pull db
               [{:entity.schema/fields [:db/id
                                        {:field/schema [:db/ident]}]}] entity-id)
       :entity.schema/fields
       (filter (fn [f]
                 (= field-id
                    (get-in f [:field/schema :db/ident]))))
       (map (fn [{:keys [:db/id]}]
              [:db/add id :field/required? required?]))
       (into [])
       ))

















