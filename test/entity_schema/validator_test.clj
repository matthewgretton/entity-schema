(ns entity-schema.validator-test
  (:require [clojure.test :refer :all]
            [entity-schema.validator :refer :all]))

;;do we want to get all the field data from the db?
(deftest get-val-test
  (testing "Test missing when required field"
    (is (= [:test-entity/string-field
            {:error/message "It is expected that :test-entity/string-field is contained in or can be derived from row {:test-entity/double-field \"bob\"}"
             :error/type    :error.type/required-field}]

           (validate-field  {:test-entity/double-field "bob"} nil
                           {:field/schema    {:db/ident     :test-entity/string-field
                                             :db/valueType :db.type/string}
                           :field/required? true}))))

  (testing "Test missing when not required field"
    (is (= [:test-entity/string-field nil]

           (validate-field  {:test-entity/double-field "bob"} nil
                           {:field/schema    {:db/ident     :test-entity/string-field
                                             :db/valueType :db.type/string}
                           :field/required? false}))))

  (testing "Test get value"
    (is (= [:test-entity/double-field 1.0]

           (validate-field
                           {:test-entity/double-field 1.0} nil
                           {:field/schema    {:db/ident     :test-entity/double-field
                                             :db/valueType :db.type/double}
                           :field/required? true}))))

  (testing "Test get value key parent style key in row"
    (is (= [:test-entity/double-field 1.0]

           (validate-field
                           {[:test-parent/test-entity :test-entity/double-field] 1.0} [:test-parent/test-entity]

                           {:field/schema    {:db/ident     :test-entity/double-field
                                             :db/valueType :db.type/double}
                           :field/required? true}))))

  (testing "Test getting value with wrong type"
    (is (= [:test-entity/double-field
            {:error/message "Type of bob expected to be :db.type/double but was class java.lang.String"
             :error/type    :error.type/incorrect-type}]

           (validate-field
                           {:test-entity/double-field "bob"} nil
                           {:field/schema    {:db/ident     :test-entity/double-field
                                             :db/valueType :db.type/double}
                           :field/required? true}))))


  (testing "Test getting a ref field"
    (is (= [:test-entity/ref-field 16]

           (with-redefs-fn {#'validate-row
                            (fn [parent-ident row schema]
                              16)}

             #(validate-field  {:test-entity/double-field "bob"} nil
                              {:field/schema        {:db/ident     :test-entity/ref-field
                                                    :db/valueType :db.type/ref}
                              :field/entity-schema {}
                              :field/required?     true}))))))





(deftest get-ref-val-test
  (testing "Simple"
    (let [entity-schema {:entity.schema/fields [{:field/schema    {:db/ident     :test-entity/string-field
                                                                   :db/valueType :db.type/string}
                                                 :field/required? true}]}]

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
                                                                                               :field/required? true}]}
                                                 :field/required?     true}]}]

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
                                                                                               :field/required? true}]}
                                                 :field/required?     true}

                                                {:field/schema        {:db/ident     :test-entity/ref-field2
                                                                       :db/valueType :db.type/ref}
                                                 :field/entity-schema {:db/ident             :entity.schema/ref-field
                                                                       :entity.schema/fields [{:field/schema    {:db/ident     :test-entity/string-field2
                                                                                                                 :db/valueType :db.type/string}
                                                                                               :field/required? true}]}
                                                 :field/required?     true}]}]

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
                                                                                               :field/required? true}]}
                                                 :field/required?     true}

                                                {:field/schema        {:db/ident     :test-entity/ref-field2
                                                                       :db/valueType :db.type/ref}
                                                 :field/entity-schema {:db/ident             :entity.schema/ref-field
                                                                       :entity.schema/fields [{:field/schema    {:db/ident     :test-entity/string-field2
                                                                                                                 :db/valueType :db.type/string}
                                                                                               :field/required? true}]}
                                                 :field/required?     true}]}]

      (is (= {:test-entity/ref-field  {:test-entity/string-field2 "Bob"}
              :test-entity/ref-field2 {:test-entity/string-field2 "Fred"}}

             (validate-row

                           {[:test-entity/ref-field :test-entity/string-field2]  "Bob"
                                     [:test-entity/ref-field2 :test-entity/string-field2] "Fred"}

                           nil


                           entity-schema))))))