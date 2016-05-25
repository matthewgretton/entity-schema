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
    :db/ident              :entity.schema/sub-type
    :db/valueType          :db.type/ref
    :db/index              true
    :db/cardinality        :db.cardinality/one}
   ]
  )




(defn pull-schema-by-id [db entity-schema-id]
  (d/pull db '[:db/ident
              { :entity.schema/type [:db/ident]}
               {:entity.schema/fields [{:field/schema [:db/ident
                                                       {:db/cardinality [:db/ident]}
                                                       {:db/valueType [:db/ident]}]}
                                       {:field/entity-schema-type [:db/ident]}
                                       :field/nullable?]}
               {:entity.schema/natural-key [:db/ident]}] entity-schema-id))


(defn pull-schema-by-type
  ([db schema-type entity-type]
   (->> (d/q '[:find ?e
               :in $ ?s-type ?e-type
               :where
               [?e :entity.schema/type ?s-type]
               [?e :entity.schema/sub-type ?e-type]] db schema-type entity-type)
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
(defn derive-schema [db schema-type entity-type]
  "Derive the schema from the entity"
  (assert (not (nil? db)))
  (assert (not (nil? schema-type)))
  (if (nil? entity-type)
    (let [pulled-schemas (pull-schema-by-type db schema-type)]
      (assert (= (count pulled-schemas) 1) (str "There is more than one schema of type " schema-type "\n"
                                                (with-out-str (clojure.pprint/pprint pulled-schemas))))
      (first pulled-schemas))
    (pull-schema-by-type db schema-type entity-type)))



;(defn derive-schema [db {{schema-type :db/ident} :field/entity-schema-type} {sub-type :entity.schema/sub-type}]
;  "Derive the schema from the entity"
;  (assert (not (nil? db)))
;  (assert (not (nil? schema-type)))
;  (if (nil? sub-type)
;    (let [pulled-schemas (pull-schema-by-type db schema-type)]
;      (assert (= (count pulled-schemas) 1) (str "There is more than one schema of type " schema-type " " pulled-schemas))
;      (first pulled-schemas))
;    (pull-schema-by-type db schema-type sub-type)))



(defn set-nullibility?-tx [db entity-id field-id nullable?]
  (->> (d/q '[:find ?f
              :in $ ?e ?s
              :where [?e :entity.schema/fields ?f]
              [?f :field/schema ?s]] db entity-id field-id)
       (first)
       (map (fn [f]
              [:db/add f :field/nullable? nullable?]))
       (first)))









