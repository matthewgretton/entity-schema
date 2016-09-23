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



(def enum-schema
  {:db/id                     (d/tempid :db.part/entity-schema)
   :db/ident                  :entity.schema/enum
   :entity.schema/part        :db.part/entity-schema
   :entity.schema/fields      #{{:field/schema    :db/ident
                                 :field/nullable? false}}
   :entity.schema/natural-key [:db/ident]})

(def datomic-field-schema
  {:db/id                (d/tempid :db.part/entity-schema)
   :db/ident             :entity.schema/datomic-field
   :entity.schema/part   :db.part/entity-schema
   :entity.schema/fields #{
                           {:db/id           (d/tempid :db.part/entity-schema)
                            :field/schema    :db/ident
                            :field/nullable? false}

                           {:db/id               (d/tempid :db.part/entity-schema)
                            :field/schema        :db/valueType
                            :field/entity-schema enum-schema
                            :field/nullable?     false}

                           {:db/id               (d/tempid :db.part/entity-schema)
                            :field/schema        :db/cardinality
                            :field/entity-schema enum-schema
                            :field/nullable?     false}

                           {:db/id           (d/tempid :db.part/entity-schema)
                            :field/schema    :db/index
                            :field/nullable? true}

                           {:db/id               (d/tempid :db.part/entity-schema)
                            :field/schema        :db/unique
                            :field/entity-schema enum-schema
                            :field/nullable?     true}
                           }
   })



(def field-schema
  {:db/ident                  :entity.schema/field
   :entity.schema/fields      #{{:field/schema        :field/schema
                                 :field/entity-schema {}
                                 :field/nullable?     false}
                                {:field/schema    :field/nullable?
                                 :field/nullable? false}}
   :entity.schema/natural-key []})


(def entity-schema-schema
  )
