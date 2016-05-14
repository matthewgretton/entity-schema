(ns entity-schema.validation-test
  (:require [clojure.test :refer :all]
            [entity-schema.validation :as v]
            [entity-schema.entity-schema :as es]
            [datomic.api :as d])
  (:import (java.util Date)))




;;do we want to get all the field data from the db?
(deftest get-val-test
  (testing "Test missing when required field"
    (is (= [:test-entity/string-field
            {:error/data    {:field     :test-entity/string-field
                             :entity-id :entity.schema/test-entity}
             :error/message "Required Field"
             :error/type    :error.type/required-field}]

           (v/validate-field nil
                             {:db/ident :entity.schema/test-entity}

                             {:field/schema    {:db/ident     :test-entity/string-field
                                                :db/valueType {:db/ident :db.type/string}}
                              :field/nullable? false}

                             {:test-entity/double-field "bob"}))))

  (testing "Test missing when not required field"
    (is (= [:test-entity/string-field nil]

           (v/validate-field nil
                             {:db/ident :entity.schema/test-entity}

                             {:field/schema    {:db/ident     :test-entity/string-field
                                                :db/valueType {:db/ident :db.type/string}}
                              :field/nullable? true}

                             {:test-entity/double-field "bob"}))))

  (testing "Test valid field validation"
    (is (= [:test-entity/double-field 1.0]

           (v/validate-field nil
                             {:db/ident :entity.schema/test-entity}

                             {:field/schema    {:db/ident     :test-entity/double-field
                                                :db/valueType {:db/ident :db.type/double}}
                              :field/nullable? false}

                             {:test-entity/double-field 1.0}))))


  (testing "Test wrong type validation"
    (is (= [:test-entity/double-field
            {:error/data    {:actual-type   java.lang.String
                             :expected-type :db.type/double
                             :value         "bob"}
             :error/message "Incorrect Value Type"
             :error/type    :error.type/incorrect-type}]

           (v/validate-field nil
                             {:db/ident :entity.schema/test-entity}

                             {:field/schema    {:db/ident     :test-entity/double-field
                                                :db/valueType {:db/ident :db.type/double}}
                              :field/nullable? false}

                             {:test-entity/double-field "bob"}))))




  (testing "Test getting a ref field"
    (is (= [:test-entity/ref-field {:a 1}]

           (with-redefs-fn {#'v/validate-entity
                            (fn [db entity schema]
                              (assert (= db "db"))
                              (assert (= entity {:a 1}))
                              (assert (= schema {:b 1}))
                              entity)
                            #'es/derive-schema
                            (fn [db entity field]
                              (assert (= db "db"))
                              (assert (= entity {:a 1}))
                              (assert (= {:field/schema
                                                               {:db/ident     :test-entity/ref-field,
                                                                :db/valueType {:db/ident :db.type/ref}},
                                          :field/entity-schema {:db/ident :some-schema},
                                          :field/nullable?     false}

                                         field))
                              {:b 1})}

             #(v/validate-field "db"
                                {:db/ident :entity.schema/test-entity}

                                {:field/schema        {:db/ident     :test-entity/ref-field
                                                       :db/valueType {:db/ident :db.type/ref}}
                                 :field/entity-schema {:db/ident :some-schema} ;;fake entity-schema
                                 :field/nullable?     false}

                                {:test-entity/double-field "bob"
                                 :test-entity/ref-field    {:a 1}}))))))



(defn create-test-db [entity-schema-tx]
  (let [conn (es/create-and-bootstrapped-conn)
        fields [{:db/id                 (d/tempid :db.part/db)
                 :db.install/_attribute :db.part/db
                 :db/ident              :test-entity/string-field
                 :db/valueType          :db.type/string
                 :db/cardinality        :db.cardinality/one}

                {:db/id                 (d/tempid :db.part/db)
                 :db.install/_attribute :db.part/db
                 :db/ident              :test-entity/string-field2
                 :db/valueType          :db.type/string
                 :db/cardinality        :db.cardinality/one}

                {:db/id                 (d/tempid :db.part/db)
                 :db.install/_attribute :db.part/db
                 :db/ident              :test-entity/test-entity2
                 :db/valueType          :db.type/ref
                 :db/cardinality        :db.cardinality/one}

                {:db/id                 (d/tempid :db.part/db)
                 :db.install/_attribute :db.part/db
                 :db/ident              :test-entity/ref-field
                 :db/valueType          :db.type/ref
                 :db/cardinality        :db.cardinality/one}
                
                {:db/id                 (d/tempid :db.part/db)
                 :db.install/_attribute :db.part/db
                 :db/ident              :test-entity/ref-many-field
                 :db/valueType          :db.type/ref
                 :db/cardinality        :db.cardinality/many}

                {:db/id                 (d/tempid :db.part/db)
                 :db.install/_attribute :db.part/db
                 :db/ident              :test-entity/string-many-field
                 :db/valueType          :db.type/string
                 :db/cardinality        :db.cardinality/many}]]

    @(d/transact conn fields)
    @(d/transact conn [entity-schema-tx])
    (d/db conn)))



(deftest get-ref-val-test
  (testing "Simple Validation Test"
    (is (= {:test-entity/string-field "Bob"}

           (-> (create-test-db {:db/id                (d/tempid :db.part/user)
                                :db/ident             :entity.schema/test-entity

                                :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
                                                        :field/schema    :test-entity/string-field
                                                        :field/nullable? false}]})

               (v/validate :entity.schema/test-entity
                           {:entity/instant           (Date.)
                            :test-entity/string-field "Bob"}

                           )))))


  (testing "Nested Validation Test"
    (is (= {:test-entity/test-entity2 {:test-entity/string-field2 "Bob"}}

           (-> {:db/id    (d/tempid :db.part/user)
                :db/ident :entity.schema/test-entity
                :entity.schema/fields
                          [{:db/id               (d/tempid :db.part/user)
                            :field/schema        :test-entity/test-entity2
                            :field/entity-schema {:db/id                (d/tempid :db.part/user)
                                                  :db/ident             :entity.schema/test-entity2
                                                  :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
                                                                          :field/schema    :test-entity/string-field2
                                                                          :field/nullable? false}]}
                            :field/nullable?     false}]}
               (create-test-db)
               (v/validate
                 :entity.schema/test-entity
                 {:entity/instant           (Date.)
                  :test-entity/test-entity2 {:db/ident                  :entity.schema/test-entity2
                                             :entity/instant            (Date.)
                                             :test-entity/string-field2 "Bob"}})))))



  (testing "Error Test"
    (is (= {:test-entity/ref-field {:error/data    {:actual-type   java.lang.String
                                                    :expected-type :db.type/ref
                                                    :value         "Bob"}
                                    :error/message "Incorrect Value Type"
                                    :error/type    :error.type/incorrect-type}}

           (-> {:db/id                (d/tempid :db.part/user)
                :db/ident             :entity.schema/test-entity
                :entity.schema/fields [{:db/id               (d/tempid :db.part/user)
                                        :field/schema        :test-entity/ref-field
                                        :field/entity-schema {:db/id                (d/tempid :db.part/user)
                                                              :db/ident             :entity.schema/ref-field
                                                              :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
                                                                                      :field/schema    :test-entity/string-field2
                                                                                      :field/nullable? false}]}
                                        :field/nullable?     false}]}
               (create-test-db)
               (v/validate

                 :entity.schema/test-entity
                 {:entity/instant        (Date.)
                  :test-entity/ref-field "Bob"})))))

  (testing "Nested Error Test"
    (is (= {:test-entity/ref-field {:test-entity/string-field {:error/data    {:actual-type   java.lang.Double
                                                                               :expected-type :db.type/string
                                                                               :value         10.0}
                                                               :error/message "Incorrect Value Type"
                                                               :error/type    :error.type/incorrect-type}}}

           (-> {:db/id                (d/tempid :db.part/user)
                :db/ident             :entity.schema/test-entity
                :entity.schema/fields [{:db/id               (d/tempid :db.part/user)
                                        :field/schema        :test-entity/ref-field
                                        :field/entity-schema {:db/id                (d/tempid :db.part/user)
                                                              :db/ident             :entity.schema/test-entity2
                                                              :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
                                                                                      :field/schema    :test-entity/string-field
                                                                                      :field/nullable? false}]}
                                        :field/nullable?     false}

                                       ]}
               (create-test-db)
               (v/validate
                 :entity.schema/test-entity
                 {:entity/instant        (Date.)
                  :test-entity/ref-field {:db/ident                 :entity.schema/test-entity2
                                          :entity/instant           (Date.)
                                          :test-entity/string-field 10.0}})))))

  (testing "Test Cardinality Many"
    (is (= {:test-entity/string-many-field #{"Bob" "Ted"}}

           (-> {:db/id                (d/tempid :db.part/user)
                :db/ident             :entity.schema/test-entity
                :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
                                        :field/schema    :test-entity/string-many-field
                                        :field/nullable? false}]}
               (create-test-db)
               (v/validate
                 :entity.schema/test-entity
                 {:entity/instant                (Date.)
                  :test-entity/string-many-field #{"Bob" "Ted"}})))))

  (testing "Test Cardinality Many Ref"
    (is (= {:test-entity/ref-many-field #{{:test-entity/string-field "Bob"}
                                          {:test-entity/string-field "Ted"}}}

           (-> {:db/id                (d/tempid :db.part/user)
                :db/ident             :entity.schema/test-entity
                :entity.schema/fields [{:db/id               (d/tempid :db.part/user)
                                        :field/schema        :test-entity/ref-many-field
                                        :field/entity-schema {:db/id                (d/tempid :db.part/user)
                                                              :db/ident             :entity.schema/test-entity2
                                                              :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
                                                                                      :field/schema    :test-entity/string-field
                                                                                      :field/nullable? false}]}
                                        :field/nullable?     false}

                                       ]}
               (create-test-db)
               (v/validate
                 :entity.schema/test-entity
                 {:entity/instant             (Date.)
                  :test-entity/ref-many-field #{{:entity/instant           (Date.)
                                                 :test-entity/string-field "Bob"}
                                                {:entity/instant           (Date.)
                                                 :test-entity/string-field "Ted"}}})))))

  (testing "Test Cardinality Many With Error"
    (is (= {:test-entity/ref-many-field #{{:test-entity/string-field "Bob"}
                                          {:test-entity/string-field {:error/data    {:actual-type   java.lang.Double
                                                                                      :expected-type :db.type/string
                                                                                      :value         10.0}
                                                                      :error/message "Incorrect Value Type"
                                                                      :error/type    :error.type/incorrect-type}}}}

           (-> {:db/id                (d/tempid :db.part/user)
                :db/ident             :entity.schema/test-entity
                :entity.schema/fields [{:db/id               (d/tempid :db.part/user)
                                        :field/schema        :test-entity/ref-many-field
                                        :field/entity-schema {:db/id                (d/tempid :db.part/user)
                                                              :db/ident             :entity.schema/test-entity2
                                                              :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
                                                                                      :field/schema    :test-entity/string-field
                                                                                      :field/nullable? false}]}
                                        :field/nullable?     false}

                                       ]}
               (create-test-db)
               (v/validate
                 :entity.schema/test-entity
                 {:entity/instant             (Date.)
                  :test-entity/ref-many-field #{{:entity/instant           (Date.)
                                                 :test-entity/string-field "Bob"}
                                                {:entity/instant           (Date.)
                                                 :test-entity/string-field 10.0}}})))))

  (testing "Pass in single value for card many field just transforms to set"
    (is (= {:test-entity/string-many-field #{"Bob"}}

           (-> {:db/id                (d/tempid :db.part/user)
                :db/ident             :entity.schema/test-entity
                :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
                                        :field/schema    :test-entity/string-many-field
                                        :field/nullable? false}]}
               (create-test-db)
               (v/validate
                 :entity.schema/test-entity
                 {:entity/instant                (Date.)
                  :test-entity/string-many-field "Bob"}))))))
