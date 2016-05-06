(ns entity-schema.validator-test
  (:require [clojure.test :refer :all]
            [entity-schema.validator :refer :all]
            [entity-schema.validation :as v]))




;;do we want to get all the field data from the db?
(deftest get-val-test
  (testing "Test missing when required field"
    (is (= [:test-entity/string-field
            {:error/data    {:field :test-entity/string-field
                             :row   {:test-entity/double-field "bob"}}
             :error/message "Non nullable field. Is expected that the field is contained or can be derived from the row"
             :error/type    :error.type/nullable-field}]

           (validate-field  {:test-entity/double-field "bob"} nil
                           {:field/schema    {:db/ident     :test-entity/string-field
                                             :db/valueType :db.type/string}
                           :field/nullable? false}))))

  (testing "Test missing when not required field"
    (is (= [:test-entity/string-field nil]

           (validate-field  {:test-entity/double-field "bob"} nil
                           {:field/schema    {:db/ident     :test-entity/string-field
                                             :db/valueType :db.type/string}
                           :field/nullable? true}))))

  (testing "Test get value"
    (is (= [:test-entity/double-field 1.0]

           (validate-field
                           {:test-entity/double-field 1.0} nil
                           {:field/schema    {:db/ident     :test-entity/double-field
                                             :db/valueType :db.type/double}
                           :field/nullable? false}))))

  (testing "Test get value key parent style key in row"
    (is (= [:test-entity/double-field 1.0]

           (validate-field
                           {[:test-parent/test-entity :test-entity/double-field] 1.0} [:test-parent/test-entity]

                           {:field/schema    {:db/ident     :test-entity/double-field
                                             :db/valueType :db.type/double}
                           :field/nullable? false}))))

  (testing "Test getting value with wrong type"
    (is (= [:test-entity/double-field
            {:error/data    {:actual-type   java.lang.String
                             :expected-type :db.type/double
                             :value         "bob"}
             :error/message "Type of value is not correct"
             :error/type    :error.type/incorrect-type}]

           (validate-field
                           {:test-entity/double-field "bob"} nil
                           {:field/schema    {:db/ident     :test-entity/double-field
                                             :db/valueType :db.type/double}
                           :field/nullable? false}))))


  (testing "Test getting a ref field"
    (is (= [:test-entity/ref-field 16]

           (with-redefs-fn {#'validate-row
                            (fn [parent-ident row schema]
                              16)}

             #(validate-field  {:test-entity/double-field "bob"} nil
                              {:field/schema        {:db/ident     :test-entity/ref-field
                                                    :db/valueType :db.type/ref}
                              :field/entity-schema {}
                              :field/nullable?     false}))))))





(deftest get-ref-val-test
  (testing "Simple"
    (let [entity-schema {:entity.schema/fields [{:field/schema    {:db/ident     :test-entity/string-field
                                                                   :db/valueType :db.type/string}
                                                 :field/nullable? false}]}]

      (is (= {:test-entity/string-field "Bob"}

             (validate-row

                           {:test-entity/string-field "Bob"}

                           nil

                           entity-schema)))))

  (testing "Nested"
    (let [entity-schema {:entity.schema/fields [{:field/schema        {:db/ident     :test-entity/ref-field
                                                                       :db/valueType :db.type/ref}
                                                 :field/entity-schema {:db/ident             :entity.schema/ref-field
                                                                       :entity.schema/fields [{:field/schema    {:db/ident     :test-entity/string-field2
                                                                                                                 :db/valueType :db.type/string}
                                                                                               :field/nullable? false}]}
                                                 :field/nullable?     false}]}]

      (is (= {:test-entity/ref-field {:test-entity/string-field2 "Bob"}}

             (validate-row

                           {:test-entity/string-field2 "Bob"}

                           nil

                           entity-schema)))))

  (testing "Nested with 2 ref fields referring to the same value"
    (let [entity-schema {:entity.schema/fields [{:field/schema        {:db/ident     :test-entity/ref-field
                                                                       :db/valueType :db.type/ref}
                                                 :field/entity-schema {:db/ident             :entity.schema/ref-field
                                                                       :entity.schema/fields [{:field/schema    {:db/ident     :test-entity/string-field2
                                                                                                                 :db/valueType :db.type/string}
                                                                                               :field/nullable? false}]}
                                                 :field/nullable?      false }

                                                {:field/schema        {:db/ident     :test-entity/ref-field2
                                                                       :db/valueType :db.type/ref}
                                                 :field/entity-schema {:db/ident             :entity.schema/ref-field
                                                                       :entity.schema/fields [{:field/schema    {:db/ident     :test-entity/string-field2
                                                                                                                 :db/valueType :db.type/string}
                                                                                               :field/nullable? false}]}
                                                 :field/nullable?      false }]}]

      (is (= {:test-entity/ref-field  {:test-entity/string-field2 "Bob"}
              :test-entity/ref-field2 {:test-entity/string-field2 "Bob"}}

             (validate-row

                           {:test-entity/string-field2 "Bob"}

                           nil

                           entity-schema)))))

  (testing "Nested entity of same type being referred to by 2 different fields"
    (let [entity-schema {:entity.schema/fields [{:field/schema        {:db/ident     :test-entity/ref-field
                                                                       :db/valueType :db.type/ref}
                                                 :field/entity-schema {:db/ident             :entity.schema/ref-field
                                                                       :entity.schema/fields [{:field/schema    {:db/ident     :test-entity/string-field2
                                                                                                                 :db/valueType :db.type/string}
                                                                                               :field/nullable? false}]}
                                                 :field/nullable?      false }

                                                {:field/schema        {:db/ident     :test-entity/ref-field2
                                                                       :db/valueType :db.type/ref}
                                                 :field/entity-schema {:db/ident             :entity.schema/ref-field
                                                                       :entity.schema/fields [{:field/schema    {:db/ident     :test-entity/string-field2
                                                                                                                 :db/valueType :db.type/string}
                                                                                               :field/nullable? false}]}
                                                 :field/nullable?      false }]}]

      (is (= {:test-entity/ref-field  {:test-entity/string-field2 "Bob"}
              :test-entity/ref-field2 {:test-entity/string-field2 "Fred"}}

             (validate-row

                           {[:test-entity/ref-field :test-entity/string-field2]  "Bob"
                                     [:test-entity/ref-field2 :test-entity/string-field2] "Fred"}

                           nil


                           entity-schema))))))