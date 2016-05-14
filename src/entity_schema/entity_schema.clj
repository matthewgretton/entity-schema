(ns entity-schema.entity-schema
  (:require [datomic.api :as d]))




(def entity-schema-fields
  [{:db/id                 (d/tempid :db.part/db)
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





(defn pull-schema [db entity-schema-id]
  (d/pull db '[:db/ident
               {:entity.schema/fields [{:field/schema [:db/ident
                                                       {:db/cardinality [:db/ident]}
                                                       {:db/valueType [:db/ident]}]}
                                       :field/nullable?
                                       {:field/entity-schema [:db/ident]}]}
               {:entity.schema/natural-key [:db/ident]}] entity-schema-id))


;;Derive schema implementations


(require 'spyscope.core)
(defn derive-schema [ db
                     {:keys [:entity/instant] :as entity}
                     {{:keys [:db/ident]} :field/entity-schema}]
  "In this example the entity instant is used to look up the correct version of the schema specified in the field"
  {:pre [(not (nil?  db)) (not (nil?  entity)) (not (nil? ident)) (not (nil? instant))]}
  (-> (d/as-of db instant)
      (pull-schema  ident)))

(defn modify-entity-schema-optionality-tx [db entity-id field-id nullable?]
  (->> (d/pull db
               [{:entity.schema/fields [:db/id
                                        {:field/schema [:db/ident]}]}] entity-id)
       :entity.schema/fields
       (filter (fn [f]
                 (= field-id
                    (get-in f [:field/schema :db/ident]))))
       (map (fn [{:keys [:db/id]}]
              [:db/add id :field/nullable? nullable?]))
       (into [])
       ))



(defn create-and-bootstrapped-conn []
  "Create an in memory database and bootstrap it with the underlying
  entity schema data"
  (let [uri (str "datomic:mem://entity-db-" (d/squuid))]
    (d/delete-database uri)
    (d/create-database uri)
    (let [conn (d/connect uri)]
      (bootstrap conn)
      conn)))













