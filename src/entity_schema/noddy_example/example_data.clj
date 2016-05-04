(ns entity-schema.example-data
  (:require [entity-schema.entity-schema :as es]
            [datomic.api :as d]))



(def example-fields [{:db/ident       :test-entity/string-field
                      :db/valueType   :db.type/string
                      :db/cardinality :db.cardinality/one}

                     {:db/ident       :test-entity/string-field2
                      :db/valueType   :db.type/string
                      :db/cardinality :db.cardinality/one}

                     {:db/ident       :test-entity/double-field
                      :db/valueType   :db.type/double
                      :db/cardinality :db.cardinality/one}

                     {:db/ident       :test-entity/ref-field
                      :db/valueType   :db.type/ref
                      :db/cardinality :db.cardinality/one}

                     {:db/ident       :test-entity/ref-field2
                      :db/valueType   :db.type/ref
                      :db/cardinality :db.cardinality/one}])

(def example-entity-schema1
  {:db/ident                 :entity.schema/test-entity1

   :entity.schema/fields     [{:field/schema    :test-entity/string-field2
                               :field/required? true}]

   :entity.schema/uniqueness [:test-entity/string-field2]})

(def example-entity-schema2
  {
   :db/ident                 :entity.schema/test-entity2

   :entity.schema/fields     [{:field/schema    :test-entity/string-field
                               :field/required? true}

                              {:field/schema        :test-entity/ref-field
                               :field/entity-schema :entity.schema/test-entity1
                               :field/required?     true}

                              {:field/schema    :test-entity/double-field
                               :field/required? false}]

   :entity.schema/uniqueness [:test-entity/ref-field :test-entity/string-field]})



(defn bootstrap-example-conn [conn]
  @(d/transact conn (es/create-fields es/entity-schema-fields))
  @(d/transact conn (es/create-fields example-fields))
  @(d/transact conn [(es/create-entity-schema example-entity-schema1)])
  @(d/transact conn [(es/create-entity-schema example-entity-schema2)])
  true)






