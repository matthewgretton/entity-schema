(ns entity-schema.datomic.entity-schema-data
  (:require [datomic.api :as d]))

(def linked-list-fields
  [{
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :list/rest
    :db/isComponent        true
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :list/first
    :db/isComponent        true
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }])

(def entity-schema-fields
  [
   ;;custom partition for schema
   {:db/id                 (d/tempid :db.part/db)
    :db/ident              :db.part/entity-schema
    :db.install/_partition :db.part/db}


   {:db/id                 (d/tempid :db.part/db)
    :db/ident              :db.part/entity-schema
    :db.install/_partition :db.part/db}

   {:db/id                 (d/tempid :db.part/db)
    :db/ident              :db.part/entity
    :db.install/_partition :db.part/db}

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
    :db/ident              :field/entity-schema
    :db/valueType          :db.type/ref
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
    :db/ident              :entity.schema/part
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/one}

   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :entity.schema/natural-key
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/one}

   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :entity.schema/type
    :db/valueType          :db.type/ref
    :db/index              true
    :db/cardinality        :db.cardinality/one}

   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :entity.schema/sub-type
    :db/valueType          :db.type/ref
    :db/index              true
    :db/cardinality        :db.cardinality/one}
   ]
  )

(def all-fields
  (->> (concat linked-list-fields entity-schema-fields)
       (into [])))