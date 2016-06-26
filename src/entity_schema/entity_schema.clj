(ns entity-schema.entity-schema
  (:require [datomic.api :as d]
            [entity-schema.datomic-helper :as dh]))


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

(defn get-schema-id-by-type
  ([db schema-type entity-type]
   (clojure.pprint/pprint "!")
   (->> (d/q '[:find ?e
               :in $ ?s-type ?e-type
               :where
               [?e :entity.schema/type ?s-type]
               [?e :entity.schema/sub-type ?e-type]] db schema-type entity-type)
        (map first)
        (first)))

  ([db schema-type]
   (clojure.pprint/pprint "!!!!!!!!!!")
   (->> (d/q '[:find ?e
               :in $ ?type
               :where [?e :entity.schema/type ?type]] db schema-type)
        (map first)
        (into #{}))))


(defn pull-schema-by-type
  ([db schema-type entity-type]
   (->> (get-schema-id-by-type db schema-type entity-type)
        (pull-schema-by-id db)))                            ;;Add check ???
  ([db schema-type]
   (->> (get-schema-id-by-type db schema-type)
        (map #(pull-schema-by-id db %))
        (into #{}))))



(defn derive-sub-type [entity]
  (:entity.schema/sub-type entity))

;; So every schema has a unique reference, it is either just the schema type or the schema type and the entity type???
(defn derive-schema "Derive the schema from the entity"
  [db field entity]
  (assert (not (nil? db)))
  (let [{{schema-type :db/ident} :field/entity-schema-type} field
        sub-type (derive-sub-type entity)]
    (assert (not (nil? schema-type)))
    (if (nil? sub-type)
      (let [pulled-schemas (pull-schema-by-type db schema-type)]
        (assert (= (count pulled-schemas) 1) (str "There is more than one schema of type " schema-type "\n"
                                                  (with-out-str (clojure.pprint/pprint pulled-schemas))))
        (first pulled-schemas))
      (pull-schema-by-type db schema-type sub-type))))


(defn set-nullibility?-tx [db entity-id field-id nullable?]
  (->> (d/q '[:find ?f
              :in $ ?e ?s
              :where [?e :entity.schema/fields ?f]
              [?f :field/schema ?s]] db entity-id field-id)
       (first)
       (map (fn [f]
              [:db/add f :field/nullable? nullable?]))
       (first)))



(defprotocol EntitySchema

  "Implementation details of database access for entity"

  (->entity-schema-by-id [id] "Pull the whole schema for the Id")

  (->db-id [partition])

  (->sub-type-id [entity] "Derive the sub-type id from the entity")

  (->entity-schema-id-by-type [schema-type] [schema-type sub-type] "Get the entity schema id/ids by type/type & sub-type")

  (->entity-id [natural-key-list entity] "Look up entity id by natural key")

  )


(defrecord Datomic-EntitySchema [db]
  EntitySchema

  (->entity-schema-by-id [id]
    (pull-schema-by-id db id))

  (->db-id [partition]
    (d/tempid partition))

  (->sub-type-id [entity] (derive-sub-type entity))

  (->entity-schema-id-by-type [schema-type]
    (get-schema-id-by-type db schema-type))

  (->entity-schema-id-by-type [schema-type sub-type] (get-schema-id-by-type db schema-type sub-type))

  (->entity-id [natural-key-list entity] (dh/look-up-entity-by-natural-key db natural-key-list entity))

  )



(defprotocol Psychodynamics
  "Plumb the inner depths of your data types"
  (thoughts [x] "The data type's innermost thoughts")
  (feelings-about [x] [x y] "Feelings about self or other"))

(defrecord Bob [])

(extend-type Bob
  Psychodynamics
  (thoughts [x] (str x " thinks, 'Truly, the character defines the data type'"))
  (feelings-about ([x] (str x " is longing for a simpler way of life"))
                  ([x y] (str x y " is longing for a simpler way of life"))))




(feelings-about (Bob.) "Ted" "Fred")




