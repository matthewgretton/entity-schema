(ns entity-schema.validation-test
  (:require [clojure.test :refer :all]
            [entity-schema.validation :as v]
            [entity-schema.entity-schema :as es]
            [entity-schema.datomic-helper :as dh]
            [datomic.api :as d])
  (:import (java.util Date)))


(deftest get-val-test
  (testing "Test missing when required field"
    (is (= [:test-entity/string-field
            {:error/data    {:field :test-entity/string-field
                             :type  :entity.schema.type/test-type}
             :error/message "Required Field"
             :error/type    :error.type/required-field}]

           (v/validate-field nil
                             {:entity.schema/type {:db/ident
                                                   :entity.schema.type/test-type}}

                             {:field/schema    {:db/ident     :test-entity/string-field
                                                :db/valueType {:db/ident :db.type/string}}
                              :field/nullable? false}

                             {:test-entity/double-field "bob"}))))

  (testing "Test missing when not required field"
    (is (= [:test-entity/string-field nil]

           (v/validate-field nil
                             {:entity.schema/type {:db/ident :entity.schema.type/test-type}}

                             {:field/schema    {:db/ident     :test-entity/string-field
                                                :db/valueType {:db/ident :db.type/string}}
                              :field/nullable? true}

                             {:test-entity/double-field "bob"}))))

  (testing "Test valid field validation"
    (is (= [:test-entity/double-field 1.0]

           (v/validate-field nil
                             {:entity.schema/type {:db/ident :entity.schema.type/test-type}}

                             {:field/schema    {:db/ident     :test-entity/double-field
                                                :db/valueType {:db/ident :db.type/double}}
                              :field/nullable? false}

                             {:test-entity/double-field 1.0}))))


  (testing "Test wrong type validation"
    (is (= [:test-entity/double-field
            {:error/data    {:datomic-type :db.type/double
                             :value-type   java.lang.String
                             :valid-types  #{java.lang.Double}
                             :value        "bob"}
             :error/message "Incorrect Value Type"
             :error/type    :error.type/incorrect-type}]

           (v/validate-field nil
                             {:entity.schema/type {:db/ident :entity.schema.type/test-type}}

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
                            (fn [db field entity]
                              (assert (not (nil? db)))
                              (assert (get-in field [:field/entity-schema-type :db/ident]))
                              (assert (get entity :a))
                              {:b 1})}

             #(v/validate-field "db"
                                {:entity.schema/type {:db/ident :entity.schema.type/test-type}}

                                {:field/schema        {:db/ident     :test-entity/ref-field
                                                       :db/valueType {:db/ident :db.type/ref}}
                                 :field/entity-schema-type {:db/ident :some-schema} ;;fake entity-schema
                                 :field/nullable?     false}

                                {:test-entity/double-field "bob"
                                 :test-entity/ref-field    {:a 1}}))))))



(defn create-db [entity-schema-txs]
  (let [uri (dh/create-in-mem-db-uri "entity-db")
        conn (do (d/create-database uri)
                 (d/connect uri))
        field-txs [{:db/id                 (d/tempid :db.part/db)
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
    @(d/transact conn es/entity-schema-fields)              ;;boot-strap
    @(d/transact conn field-txs)
    @(d/transact conn entity-schema-txs)
    (d/db conn)))


(deftest get-ref-val-test
  (testing "Simple Validation Test"
    (is (= {:test-entity/string-field "Bob"}

           (-> (create-db [{:db/id                (d/tempid :db.part/user)
                            :entity.schema/type   {:db/id    (d/tempid :db.part/user)
                                                   :db/ident :entity.schema.type/test-type}

                            :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
                                                    :field/schema    :test-entity/string-field
                                                    :field/nullable? false}]}])

               (v/validate
                 :entity.schema.type/test-type
                 {:test-entity/string-field "Bob"})))))


  (testing "Nested Validation Test"
    (is (= {:test-entity/test-entity2 {:test-entity/string-field2 "Bob"}}

           (-> (create-db
                 [{:db/id              (d/tempid :db.part/user)
                   :entity.schema/type {:db/id    (d/tempid :db.part/user)
                                        :db/ident :entity.schema.type/test-type}
                   :entity.schema/fields
                                       [{:db/id                    (d/tempid :db.part/user)
                                         :field/schema             :test-entity/test-entity2
                                         :field/entity-schema-type {:db/id    (d/tempid :db.part/user -1)
                                                                    :db/ident :entity.schema.type/test-type2}
                                         :field/nullable?          false}]}
                  {:db/id                (d/tempid :db.part/user)
                   :entity.schema/type   (d/tempid :db.part/user -1)
                   ;; note the custom id definted, this ensures  entity created above links to this
                   :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
                                           :field/schema    :test-entity/string-field2
                                           :field/nullable? false}]}])
               (v/validate
                 :entity.schema.type/test-type
                 {:test-entity/test-entity2 {:entity/instant            (Date.)
                                             :test-entity/string-field2 "Bob"}})))))



  (testing "Error Test"
    (is (= {:test-entity/ref-field {:error/data    {:datomic-type :db.type/ref
                                                    :valid-types  #{clojure.lang.Keyword
                                                                    java.util.Map}
                                                    :value        "Bob"
                                                    :value-type   java.lang.String}
                                    :error/message "Incorrect Value Type"
                                    :error/type    :error.type/incorrect-type}}

           (-> (create-db [{:db/id                (d/tempid :db.part/user)
                            :entity.schema/type   {:db/id    (d/tempid :db.part/user)
                                                   :db/ident :entity.schema.type/test-type}
                            :entity.schema/fields [{:db/id                    (d/tempid :db.part/user)
                                                    :field/schema             :test-entity/ref-field
                                                    :field/entity-schema-type {:db/id    (d/tempid :db.part/user -1)
                                                                               :db/ident :entity.schema.type/test-type2}
                                                    :field/nullable?          false}]}
                           {:db/id                (d/tempid :db.part/user)
                            :entity.schema/type   (d/tempid :db.part/user -1)
                            :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
                                                    :field/schema    :test-entity/string-field2
                                                    :field/nullable? false}]}])
               (v/validate

                 :entity.schema.type/test-type
                 {:entity/schema         :entity.schema/test-entity
                  :test-entity/ref-field "Bob"})))))

  (testing "Nested Error Test"
    (is (= {:test-entity/ref-field {:test-entity/string-field
                                    {:error/data    {:datomic-type :db.type/string
                                                     :valid-types  #{java.lang.String}
                                                     :value        10.0
                                                     :value-type   java.lang.Double}
                                     :error/message "Incorrect Value Type"
                                     :error/type    :error.type/incorrect-type}}}

           (-> (create-db
                 [{:db/id                (d/tempid :db.part/user)
                   :entity.schema/type   {:db/id    (d/tempid :db.part/user)
                                          :db/ident :entity.schema.type/test-type}
                   :entity.schema/fields [{:db/id                    (d/tempid :db.part/user)
                                           :field/schema             :test-entity/ref-field
                                           :field/entity-schema-type {:db/id    (d/tempid :db.part/user -1)
                                                                      :db/ident :entity.schema.type/test-type2}
                                           :field/nullable?          false}]}
                  {:db/id                (d/tempid :db.part/user)
                   :entity.schema/type   (d/tempid :db.part/user -1)
                   :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
                                           :field/schema    :test-entity/string-field
                                           :field/nullable? false}]}])
               (v/validate
                 :entity.schema.type/test-type
                 {:test-entity/ref-field {:entity/instant           (Date.)
                                          :entity/schema            :entity.schema/test-entity2
                                          :test-entity/string-field 10.0}})))))

  (testing "Test Cardinality Many"
    (is (= {:test-entity/string-many-field #{"Bob" "Ted"}}

           (-> (create-db [{:db/id                (d/tempid :db.part/user)
                            :entity.schema/type   {:db/id    (d/tempid :db.part/user)
                                                   :db/ident :entity.schema.type/test-type}
                            :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
                                                    :field/schema    :test-entity/string-many-field
                                                    :field/nullable? false}]}])
               (v/validate
                 :entity.schema.type/test-type
                 {:test-entity/string-many-field #{"Bob" "Ted"}})))))

  (testing "Test Cardinality Many Ref"
    (is (= {:test-entity/ref-many-field #{{:test-entity/string-field "Bob"}
                                          {:test-entity/string-field "Ted"}}}

           (-> (create-db [{:db/id                (d/tempid :db.part/user)
                            :entity.schema/type   {:db/id    (d/tempid :db.part/user)
                                                   :db/ident :entity.schema.type/test-type}
                            :entity.schema/fields [{:db/id                    (d/tempid :db.part/user)
                                                    :field/schema             :test-entity/ref-many-field
                                                    :field/entity-schema-type {:db/id    (d/tempid :db.part/user -1)
                                                                               :db/ident :entity.schema.type/test-type2}
                                                    :field/nullable?          false}]}
                           {:db/id                (d/tempid :db.part/user)
                            :entity.schema/type   (d/tempid :db.part/user -1)
                            :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
                                                    :field/schema    :test-entity/string-field
                                                    :field/nullable? false}]}])
               (v/validate
                 :entity.schema.type/test-type
                 {:test-entity/ref-many-field #{{:entity/instant           (Date.)
                                                 :entity/schema            :entity.schema/test-entity2
                                                 :test-entity/string-field "Bob"}
                                                {:entity/instant           (Date.)
                                                 :entity/schema            :entity.schema/test-entity2
                                                 :test-entity/string-field "Ted"}}})))))

  (testing "Test Cardinality Many With Error"
    (is (= {:test-entity/ref-many-field #{{:test-entity/string-field "Bob"}
                                          {:test-entity/string-field {:error/data    {:datomic-type :db.type/string
                                                                                      :valid-types  #{java.lang.String}
                                                                                      :value        10.0
                                                                                      :value-type   java.lang.Double}
                                                                      :error/message "Incorrect Value Type"
                                                                      :error/type    :error.type/incorrect-type}}}}

           (-> (create-db [{:db/id                (d/tempid :db.part/user)
                            :entity.schema/type   {:db/id    (d/tempid :db.part/user)
                                                   :db/ident :entity.schema.type/test-type}
                            :entity.schema/fields [{:db/id                    (d/tempid :db.part/user)
                                                    :field/schema             :test-entity/ref-many-field
                                                    :field/entity-schema-type {:db/id    (d/tempid :db.part/user -1)
                                                                               :db/ident :entity.schema.type/test-type2}
                                                    :field/nullable?          false}]}
                           {:db/id                (d/tempid :db.part/user)
                            :entity.schema/type   (d/tempid :db.part/user -1)
                            :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
                                                    :field/schema    :test-entity/string-field
                                                    :field/nullable? false}]}])
               (v/validate
                 :entity.schema.type/test-type
                 {:test-entity/ref-many-field #{{:entity/instant           (Date.)
                                                 :entity/schema            :entity.schema/test-entity2
                                                 :test-entity/string-field "Bob"}
                                                {:entity/schema            :entity.schema/test-entity2
                                                 :entity/instant           (Date.)
                                                 :test-entity/string-field 10.0}}})))))

  (testing "Pass in single value for card many field just transforms to set"
    (is (= {:test-entity/string-many-field #{"Bob"}}

           (-> (create-db [{:db/id                (d/tempid :db.part/user)
                            :entity.schema/type   {:db/id    (d/tempid :db.part/user)
                                                   :db/ident :entity.schema.type/test-type}
                            :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
                                                    :field/schema    :test-entity/string-many-field
                                                    :field/nullable? false}]}])
               (v/validate
                 :entity.schema.type/test-type
                 {:test-entity/string-many-field "Bob"})))))

  (testing "Test matching multiple versons of one shcema"
    (is (= {:test-entity/string-field "Bob"}

           (-> (create-db [{:db/id                  (d/tempid :db.part/user)

                            :entity.schema/type     {:db/id    (d/tempid :db.part/user -1)
                                                     :db/ident :entity.schema.type/test-type}
                            :entity.schema/sub-type {:db/id    (d/tempid :db.part/user)
                                                     :db/ident :entity.type/test-type}
                            :entity.schema/fields   [{:db/id           (d/tempid :db.part/user)
                                                      :field/schema    :test-entity/string-field
                                                      :field/nullable? false}]}

                           {:db/id                  (d/tempid :db.part/user)

                            :entity.schema/type     {:db/id    (d/tempid :db.part/user -1)
                                                     :db/ident :entity.schema.type/test-type}

                            :entity.schema/sub-type {:db/id    (d/tempid :db.part/user)
                                                     :db/ident :entity.type/test-type2}

                            :entity.schema/fields   [{:db/id           (d/tempid :db.part/user)
                                                      :field/schema    :test-entity/string-field
                                                      :field/nullable? false}
                                                     {:db/id           (d/tempid :db.part/user)
                                                      :field/schema    :test-entity/string-field2
                                                      :field/nullable? false}]}])
               (v/validate
                 :entity.schema.type/test-type
                 {:entity.schema/sub-type   :entity.type/test-type
                  :test-entity/string-field "Bob"})))))

  (testing "Test Error on not suppying enough info to resolve schema"
    (is (thrown-with-msg? AssertionError #"Assert failed: There is more than one schema of type"
                          (= {:test-entity/string-field "Bob"}

                             (-> (create-db [{:db/id                  (d/tempid :db.part/user)

                                              :entity.schema/type     {:db/id    (d/tempid :db.part/user -1)
                                                                       :db/ident :entity.schema.type/test-type}
                                              :entity.schema/sub-type {:db/id    (d/tempid :db.part/user)
                                                                       :db/ident :entity.type/test-type}
                                              :entity.schema/fields   [{:db/id           (d/tempid :db.part/user)
                                                                        :field/schema    :test-entity/string-field
                                                                        :field/nullable? false}]}

                                             {:db/id                  (d/tempid :db.part/user)

                                              :entity.schema/type     {:db/id    (d/tempid :db.part/user -1)
                                                                       :db/ident :entity.schema.type/test-type}

                                              :entity.schema/sub-type {:db/id    (d/tempid :db.part/user)
                                                                       :db/ident :entity.type/test-type2}

                                              :entity.schema/fields   [{:db/id           (d/tempid :db.part/user)
                                                                        :field/schema    :test-entity/string-field
                                                                        :field/nullable? false}
                                                                       {:db/id           (d/tempid :db.part/user)
                                                                        :field/schema    :test-entity/string-field2
                                                                        :field/nullable? false}]}])
                                 (v/validate
                                   :entity.schema.type/test-type
                                   {:test-entity/string-field "Bob"})))))))
