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
                             :entity-id :entity.schema/test}
             :error/message "Required Field"
             :error/type    :error.type/nullable-field}]

           (v/validate-field nil :entity.schema/test {:test-entity/double-field "bob"}
                             {:field/schema    {:db/ident     :test-entity/string-field
                                                :db/valueType {:db/ident :db.type/string}}
                              :field/nullable? false}))))

  (testing "Test missing when not required field"
    (is (= [:test-entity/string-field nil]

           (v/validate-field nil :entity.schema/test {:test-entity/double-field "bob"}
                             {:field/schema    {:db/ident     :test-entity/string-field
                                                :db/valueType {:db/ident :db.type/string}}
                              :field/nullable? true}))))

  (testing "Test get value"
    (is (= [:test-entity/double-field 1.0]

           (v/validate-field nil :entity.schema/test
                             {:test-entity/double-field 1.0}
                             {:field/schema    {:db/ident     :test-entity/double-field
                                                :db/valueType {:db/ident :db.type/double}}
                              :field/nullable? false}))))

  (testing "Test get value key parent style key in row"
    (is (= [:test-entity/double-field 1.0]

           (v/validate-field nil :entity.schema/test
                             {:test-entity/double-field 1.0}

                             {:field/schema    {:db/ident     :test-entity/double-field
                                                :db/valueType {:db/ident :db.type/double}}
                              :field/nullable? false}))))

  (testing "Test getting value with wrong type"
    (is (= [:test-entity/double-field
            {:error/data    {:actual-type   java.lang.String
                             :expected-type :db.type/double
                             :value         "bob"}
             :error/message "Incorrect Value Type"
             :error/type    :error.type/incorrect-type}]

           (v/validate-field nil :entity.schema/test
                             {:test-entity/double-field "bob"}
                             {:field/schema    {:db/ident     :test-entity/double-field
                                                :db/valueType {:db/ident :db.type/double}}
                              :field/nullable? false}))))


  (testing "Test getting a ref field"
    (is (= [:test-entity/ref-field 16]

           (with-redefs-fn {#'v/validate-entity
                            (fn [db row schema]
                              16)
                            #'es/derive-schema
                            (fn [db entity]
                              {})}

             #(v/validate-field "db" :entity.schema/test {:test-entity/double-field "bob"
                                                          :test-entity/ref-field    {}}
                                {:field/schema        {:db/ident     :test-entity/ref-field
                                                       :db/valueType {:db/ident :db.type/ref}}
                                 :field/entity-schema {}
                                 :field/nullable?     false})))))

  (testing "Test getting a ref field"
    (is (= [:test-entity/ref-field {:a 1}]

           (with-redefs-fn {#'v/validate-entity
                            (fn [db row schema]
                              (clojure.pprint/pprint (str "!!!!" schema))
                              (assert (= schema {}))
                              (assert (= db "db"))
                              row)
                            #'es/derive-schema
                            (fn [db entity]
                              (assert (= db "db"))
                              {})}

             #(v/validate-field "db" :entity.schema/test {:test-entity/double-field "bob"
                                                          :test-entity/ref-field    {:a 1}}
                                {:field/schema        {:db/ident     :test-entity/ref-field
                                                       :db/valueType {:db/ident :db.type/ref}}
                                 :field/entity-schema {:db/ident :some-schema} ;;fake entity-schema
                                 :field/nullable?     false}))))))



(defn create-test-db [entity-schema-tx]
  (let [conn (es/create-bootstrapped-conn)
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
                 :db/cardinality        :db.cardinality/one}]]

    @(d/transact conn fields)
    @(d/transact conn [entity-schema-tx])
    (d/db conn)))



(deftest get-ref-val-test
  (testing "Simple"
    (is (= {:test-entity/string-field "Bob"}

           (v/validate (create-test-db {:db/id                (d/tempid :db.part/user)
                                        :db/ident             :entity.schema/test-entity

                                        :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
                                                                :field/schema    :test-entity/string-field
                                                                :field/nullable? false}]})

                       {:db/ident                 :entity.schema/test-entity
                        :event/instant            (Date.)
                        :test-entity/string-field "Bob"}

                       ))))


  (testing "Nested"
    (is (= {:test-entity/test-entity2 {:test-entity/string-field2 "Bob"}}

           (v/validate (create-test-db {:db/id                (d/tempid :db.part/user)
                                        :db/ident             :entity.schema/test-entity
                                        :entity.schema/fields [{:db/id               (d/tempid :db.part/user)
                                                                :field/schema        :test-entity/test-entity2
                                                                :field/entity-schema {:db/id (d/tempid :db.part/user)
                                                                                      :db/ident             :entity.schema/test-entity2
                                                                                      :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
                                                                                                              :field/schema    :test-entity/string-field2
                                                                                                              :field/nullable? false}]}
                                                                :field/nullable?     false}]})

                       {:db/ident              :entity.schema/test-entity
                        :event/instant         (Date.)
                        :test-entity/test-entity2 {:db/ident :entity.schema/test-entity2
                                                   :event/instant (Date.)
                                                   :test-entity/string-field2 "Bob"}}

                       ))))



  (testing "Type error"
    (is (= {:test-entity/ref-field {:error/data    {:actual-type   java.lang.String
                                                    :expected-type :db.type/ref
                                                    :value         "Bob"}
                                    :error/message "Incorrect Value Type"
                                    :error/type    :error.type/incorrect-type}}

           (v/validate-entity
             (create-test-db {:db/id                (d/tempid :db.part/user)
                              :db/ident             :entity.schema/test-entity
                              :entity.schema/fields [{:field/schema        {:db/ident     :test-entity/ref-field
                                                                            :db/valueType {:db/ident :db.type/ref}}
                                                      :field/entity-schema {:db/ident             :entity.schema/ref-field
                                                                            :entity.schema/fields [{:field/schema    {:db/ident     :test-entity/string-field2
                                                                                                                      :db/valueType {:db/ident :db.type/string}}
                                                                                                    :field/nullable? false}]}
                                                      :field/nullable?     false}

                                                     ]})

             {:db/ident              :entity.schema/test-entity
              :event/instant         (Date.)
              :test-entity/ref-field "Bob"}))))

  (testing "Nested error"
    (let [entity-schema {:entity.schema/fields [{:field/schema        {:db/ident     :test-entity/ref-field
                                                                       :db/valueType {:db/ident :db.type/ref}}
                                                 :field/entity-schema {:db/ident             :entity.schema/ref-field
                                                                       :entity.schema/fields [{:field/schema    {:db/ident     :test-entity/string-field
                                                                                                                 :db/valueType {:db/ident :db.type/string}}
                                                                                               :field/nullable? false}]}
                                                 :field/nullable?     false}

                                                ]}]

      (is (= {:test-entity/ref-field {:error/data    {:actual-type   java.lang.Double
                                                      :expected-type :db.type/string
                                                      :value         "Bob"}
                                      :error/message "Incorrect Value Type"
                                      :error/type    :error.type/incorrect-type}}

             (v/validate-entity (create-test-db)

                                {:test-entity/ref-field {:test-entity/string-field 10}}



                                entity-schema))))))
