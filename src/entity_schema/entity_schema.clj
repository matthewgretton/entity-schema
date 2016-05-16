(ns entity-schema.entity-schema
  (:require [datomic.api :as d]))




(def entity-schema-fields
  [
   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :field/schema
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/one}

   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :field/nullable?
    :db/valueType          :db.type/boolean
    :db/cardinality        :db.cardinality/one}

   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :field/entity-schema-type
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
    :db/cardinality        :db.cardinality/many}

   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :entity.schema/type
    :db/valueType          :db.type/ref
    :db/index              true
    :db/cardinality        :db.cardinality/one}

   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :entity.schema/entity-type
    :db/valueType          :db.type/ref
    :db/index              true
    :db/cardinality        :db.cardinality/one}
   ]
  )




(defn pull-schema-by-id [db entity-schema-id]
  (d/pull db '[:db/ident
               {:entity.schema/fields [{:field/schema [:db/ident
                                                       {:db/cardinality [:db/ident]}
                                                       {:db/valueType [:db/ident]}]}
                                       {:field/entity-schema-type [:db/ident]}
                                       :field/nullable?]}
               {:entity.schema/natural-key [:db/ident]}] entity-schema-id))


(defn pull-schema
  ([db schema-type entity-type]
   (->> (d/q '[:find ?e
               :in $ ?s-type ?e-type
               :where
               [?e :entity.schema/type ?s-type]
               [?e :entity.schema/entity-type ?e-type]] db schema-type entity-type)
        (map first)
        (map (partial pull-schema-by-id db))
        (first)))                                           ;;Add check ???
  ([db schema-type]
   (->> (d/q '[:find ?e
               :in $ ?type
               :where [?e :entity.schema/type ?type]] db schema-type)
        (map first)
        (map (partial pull-schema-by-id db))
        (into #{}))))


;; So every schema has a unique reference, it is either just the schema type or the schema type and the entity type???
(defn derive-schema [db
                     instant
                     schema-type
                     entity-type]
  "Derive the schema from the entity"
  {:pre [(not (nil? db)) (not (nil? schema-type)) (not (nil? instant))]}

  (let [db-instant (d/as-of db instant)]
    ;the database asof the time in the entity
    (if (nil? entity-type)
      (let [pulled-schemas (pull-schema db-instant schema-type)]
        (assert (= (count pulled-schemas) 1) (str "There is more than one schema of type " schema-type " " pulled-schemas))
        (first pulled-schemas))
      (pull-schema db schema-type entity-type))))













