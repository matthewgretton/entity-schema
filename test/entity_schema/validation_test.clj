(ns entity-schema.validation-test
  (:require [clojure.test :refer :all]
            [datomic.api :as d]
            [entity-schema.processor :as p]
            [entity-schema.test-utils :as test]))

(def card-one-unique-string-field
  {:db/ident       :test-entity/string-field
   :db/valueType   :db.type/string
   :db/cardinality :db.cardinality/one
   :db/unique      :db.unique/identity})

(def schema-with-required-unqiue-field
  {:db/ident                  :entity.schema/test
   :entity.schema/part        :db.part/entity-schema
   :entity.schema/fields      [{:field/nullable? false
                                :field/schema    :test-entity/string-field}]
   :entity.schema/natural-key [:test-entity/string-field]})

(def schema-with-optional-unqiue-field
  {:db/ident                  :entity.schema/test
   :entity.schema/part        :db.part/entity-schema
   :entity.schema/fields      [{:field/nullable? true
                                :field/schema    :test-entity/string-field}]
   :entity.schema/natural-key [:test-entity/string-field]})

(def insert-but-look-up-by-defualt-command-map
  {:command-map     {:entity.schema/test :command/insert}
   :default-command :command/look-up})


(deftest multiple-entity-validation-tests
  (->> (test/create-comparable-output
         "Valid Test"
         [card-one-unique-string-field]

         {:db/ident                  :entity.schema/test
          :entity.schema/part        :db.part/entity-schema
          :entity.schema/fields      [{:field/nullable? true
                                       :field/schema    :test-entity/string-field}]
          :entity.schema/natural-key [:test-entity/string-field]}

         insert-but-look-up-by-defualt-command-map

         [{:test-entity/string-field "Bob12"}
          {:test-entity/string-field "Bob12"}]

         [[[{:test-entity/string-field "Bob12"
             :db/id                    (d/tempid :db.part/entity-schema -1)} false]
           [{:test-entity/string-field "Bob12"
             :db/id                    (d/tempid :db.part/entity-schema -1)} false]
           ] false])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (test/create-comparable-output
         "Valid"
         [card-one-unique-string-field] schema-with-required-unqiue-field
         insert-but-look-up-by-defualt-command-map
         [{:test-entity/string-field "Bob12"}]
         [[[{:test-entity/string-field "Bob12"
             :db/id                    (d/tempid :db.part/entity-schema)} false]] false])
       ((fn [[exp act desc]] (is (= exp act) desc)))))

(deftest single-entity-validation-tests

  (->> (test/create-comparable-valid-single-entity-output
         "Valid"
         [card-one-unique-string-field] schema-with-required-unqiue-field
         insert-but-look-up-by-defualt-command-map
         {:test-entity/string-field "Bob12"}
         {:db/id                    (d/tempid :db.part/entity-schema)
          :test-entity/string-field "Bob12"})
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (test/create-comparable-invalid-single-entity-output
         "Incorrect field type"
         [card-one-unique-string-field]
         schema-with-required-unqiue-field
         insert-but-look-up-by-defualt-command-map

         {:test-entity/string-field 12}

         {:test-entity/string-field
          {:error/data    {:datomic-type :db.type/string
                           :valid-types  #{java.lang.String}
                           :value        12
                           :value-type   java.lang.Long}
           :error/message "Incorrect Value Type"
           :error/type    :error.type/incorrect-type}})
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (test/create-comparable-invalid-single-entity-output
         "Missing required field"
         [card-one-unique-string-field]
         schema-with-required-unqiue-field
         insert-but-look-up-by-defualt-command-map
         {}
         {:test-entity/string-field {:error/message "Required Field"
                                     :error/type    :error.type/required-field}})
       ((fn [[exp act desc]] (is (= exp act) desc))))




  (->> (test/create-comparable-invalid-single-entity-output
         "Invalid Function Test"
         [card-one-unique-string-field]
         {:db/ident                  :entity.schema/test
          :entity.schema/part        :db.part/entity-schema
          :entity.schema/fields      [{:field/nullable?           true
                                       :field/schema              :test-entity/string-field
                                       :field/validation-function {:db/ident :some-func
                                                                   :db/fn    (d/function {:lang   :clojure
                                                                                          :params '[value]
                                                                                          :code   '(<= (count value) 5)})}}]
          :entity.schema/natural-key [:test-entity/string-field]}
         insert-but-look-up-by-defualt-command-map
         {:test-entity/string-field "More Than 5 chars"}
         {:test-entity/string-field {:error/data    {:validation-func {:lang     :clojure
                                                                       :imports  []
                                                                       :requires []
                                                                       :params   '[value]
                                                                       :code     "(<= (count value) 5)"}
                                                     :value           "More Than 5 chars"}
                                     :error/message "Validation Function has failed"
                                     :error/type    :error.type/validation-function}})
       ((fn [[exp act desc]] (is (= exp act) desc))))


  (->> (test/create-comparable-valid-single-entity-output
         "Valid Function Test"
         [card-one-unique-string-field]
         {:db/ident                  :entity.schema/test
          :entity.schema/part        :db.part/entity-schema
          :entity.schema/fields      [{:field/nullable?           true
                                       :field/schema              :test-entity/string-field
                                       :field/validation-function {:db/ident :some-func
                                                                   :db/fn    (d/function {:lang   :clojure
                                                                                          :params '[value]
                                                                                          :code   '(<= (count value) 5)})}}]
          :entity.schema/natural-key [:test-entity/string-field]}
         insert-but-look-up-by-defualt-command-map
         {:test-entity/string-field "<5"}
         {:db/id (d/tempid :db.part/entity-schema) :test-entity/string-field "<5"})
       ((fn [[exp act desc]] (is (= exp act) desc))))

  ;; What do we need to happen here?
  (->> (test/create-comparable-invalid-single-entity-output
         "Missing (optional) natural key"
         [card-one-unique-string-field] schema-with-optional-unqiue-field
         insert-but-look-up-by-defualt-command-map
         {}
         {:db/id {:error/message "Missing natural key"
                  :error/type    :error.type/missing-natrual-key
                  :error/data    {:error/entity      {}
                                  :error/natural-key [:test-entity/string-field]}}})
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (test/create-comparable-invalid-single-entity-output
         "Cardinality Test"
         [card-one-unique-string-field]
         {:db/ident                  :entity.schema/test
          :entity.schema/part        :db.part/entity-schema
          :entity.schema/fields      [{:field/nullable? true
                                       :field/schema    :test-entity/string-field}]
          :entity.schema/natural-key [:test-entity/string-field]}
         insert-but-look-up-by-defualt-command-map
         {:test-entity/string-field ["Bob" "Ted"]}
         {:test-entity/string-field {:error/data    {:value ["Bob"
                                                             "Ted"]}
                                     :error/message "Value associated with cardinality one field should not be a collection"
                                     :error/type    :error.type/cardinality}})
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (test/create-comparable-valid-single-entity-output
         "Single Value input for Cardinality many field should be fine"
         [{:db/ident       :test-entity/string-field
           :db/valueType   :db.type/string
           :db/cardinality :db.cardinality/many
           :db/unique      :db.unique/identity}]
         {:db/ident                  :entity.schema/test
          :entity.schema/part        :db.part/entity-schema
          :entity.schema/fields      [{:field/nullable? true
                                       :field/schema    :test-entity/string-field}]
          :entity.schema/natural-key [:test-entity/string-field]} insert-but-look-up-by-defualt-command-map
         {:test-entity/string-field "Bob"}
         {:db/id                    (d/tempid :db.part/entity-schema)
          :test-entity/string-field #{"Bob"}})
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (test/create-comparable-valid-single-entity-output

         "Test expanding entity"
         [{:db/ident       :test-entity/ref-field
           :db/valueType   :db.type/ref
           :db/cardinality :db.cardinality/one
           :db/unique      :db.unique/identity}
          {:db/ident       :test-entity/string-field
           :db/valueType   :db.type/string
           :db/cardinality :db.cardinality/one
           :db/unique      :db.unique/identity}]
         {:db/ident                  :entity.schema/test
          :entity.schema/part        :db.part/user
          :entity.schema/fields      [{:field/nullable?     true
                                       :field/schema        :test-entity/ref-field
                                       :field/entity-schema {:db/ident                  :entity.schema/internal
                                                             :entity.schema/part        :db.part/user
                                                             :entity.schema/fields      [{:field/nullable? true
                                                                                          :field/schema    :test-entity/string-field}]
                                                             :entity.schema/natural-key [:test-entity/string-field]}}]
          :entity.schema/natural-key [:test-entity/ref-field]}
         insert-but-look-up-by-defualt-command-map

         {:test-entity/ref-field :bob-entity}

         {:db/id                 (d/tempid :db.part/user)
          :test-entity/ref-field {:db/id                    :bob-entity
                                  :test-entity/string-field "Bob"}}


         [{:db/id                    (d/tempid :db.part/user)
           :db/ident                 :bob-entity
           :test-entity/string-field "Bob"}])
       ((fn [[exp act desc]] (is (= exp act) desc)))))


(deftest test-combine
  (let [left-input
        [[[{:db/id           (d/tempid :db.part/user -1)
            :entity/code     {:db/id 123 :code/value "A"}
            :entity/unq-key1 "Bob"
            :entity/unq-key2 2} false]]

         {:entity.schema/test1
          {{:entity/unq-key1 "Bob"
            :entity/unq-key2 2}
           [false (d/tempid :db.part/user -1)],
           }
          :entity.schema/test2
          {{:code/value "A"}
           [true 123],
           }}
         nil
         false
         ]
        ;;TODO add one in that comes from the db and then try also with ones that do/don't exist on the
        ;;TODO right or left hand side.
        right-input [[[{:db/id           (d/tempid :db.part/user -2)
                        :entity/code     {:db/id 123 :code/value "A"}
                        :entity/unq-key1 "Bob"
                        :entity/unq-key2 2} false]
                      ]
                     {:entity.schema/test1
                      {{:entity/unq-key1 "Bob"
                        :entity/unq-key2 2}
                       [false (d/tempid :db.part/user -2)],
                       }
                      :entity.schema/test2
                      {{:code/value "A"}
                       [true 123],
                       }}
                     nil
                     false]
        result (p/combine-result left-input right-input)
        top-id (:db/id (first (first (first result))))
        expected [[[{:db/id           top-id
                     :entity/code     {:db/id 123, :code/value "A"},
                     :entity/unq-key1 "Bob",
                     :entity/unq-key2 2}
                    false]
                   [{:db/id           top-id
                     :entity/code     {:db/id 123, :code/value "A"},
                     :entity/unq-key1 "Bob",
                     :entity/unq-key2 2}
                    false]]
                  {:entity.schema/test1
                   {{:entity/unq-key1 "Bob", :entity/unq-key2 2}
                    [false top-id]},
                   :entity.schema/test2
                   {{:code/value "A"}
                    [true 123]}}
                  nil
                  false]]
    (is (= expected result))))









;
;
;
;
;
;
;
;(defn create-db [entity-schema-txs]
;  (let [uri (dh/create-in-mem-db-uri "entity-db")
;        conn (do (d/create-database uri)
;                 (d/connect uri))
;        field-txs [{:db/id                 (d/tempid :db.part/db)
;                    :db.install/_attribute :db.part/db
;                    :db/ident              :test-entity/string-field
;                    :db/valueType          :db.type/string
;                    :db/cardinality        :db.cardinality/one}
;
;                   {:db/id                 (d/tempid :db.part/db)
;                    :db.install/_attribute :db.part/db
;                    :db/ident              :test-entity/string-field2
;                    :db/valueType          :db.type/string
;                    :db/cardinality        :db.cardinality/one}
;
;                   {:db/id                 (d/tempid :db.part/db)
;                    :db.install/_attribute :db.part/db
;                    :db/ident              :test-entity/test-entity2
;                    :db/valueType          :db.type/ref
;                    :db/cardinality        :db.cardinality/one}
;
;                   {:db/id                 (d/tempid :db.part/db)
;                    :db.install/_attribute :db.part/db
;                    :db/ident              :test-entity/ref-field
;                    :db/valueType          :db.type/ref
;                    :db/cardinality        :db.cardinality/one}
;
;                   {:db/id                 (d/tempid :db.part/db)
;                    :db.install/_attribute :db.part/db
;                    :db/ident              :test-entity/ref-many-field
;                    :db/valueType          :db.type/ref
;                    :db/cardinality        :db.cardinality/many}
;
;                   {:db/id                 (d/tempid :db.part/db)
;                    :db.install/_attribute :db.part/db
;                    :db/ident              :test-entity/string-many-field
;                    :db/valueType          :db.type/string
;                    :db/cardinality        :db.cardinality/many}]]
;    @(d/transact conn es/all-fields)                        ;;boot-strap
;    @(d/transact conn field-txs)
;    @(d/transact conn entity-schema-txs)
;    (d/db conn)))
;
;
;(deftest get-ref-val-test
;  (testing "Simple Validation Test"
;    (is (= {:test-entity/string-field "Bob"}
;
;           (-> (create-db [{:db/id                (d/tempid :db.part/user)
;                            :entity.schema/type   {:db/id    (d/tempid :db.part/user)
;                                                   :db/ident :entity.schema.type/test-type}
;
;                            :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
;                                                    :field/schema    :test-entity/string-field
;                                                    :field/nullable? false}]}])
;
;               (v/validate
;                 :entity.schema.type/test-type
;                 {:test-entity/string-field "Bob"})))))
;
;
;  (testing "Nested Validation Test"
;    (is (= {:test-entity/test-entity2 {:test-entity/string-field2 "Bob"}}
;
;           (-> (create-db
;                 [{:db/id              (d/tempid :db.part/user)
;                   :entity.schema/type {:db/id    (d/tempid :db.part/user)
;                                        :db/ident :entity.schema.type/test-type}
;                   :entity.schema/fields
;                                       [{:db/id                    (d/tempid :db.part/user)
;                                         :field/schema             :test-entity/test-entity2
;                                         :field/entity-schema-type {:db/id    (d/tempid :db.part/user -1)
;                                                                    :db/ident :entity.schema.type/test-type2}
;                                         :field/nullable?          false}]}
;                  {:db/id                (d/tempid :db.part/user)
;                   :entity.schema/type   (d/tempid :db.part/user -1)
;                   ;; note the custom id definted, this ensures  entity created above links to this
;                   :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
;                                           :field/schema    :test-entity/string-field2
;                                           :field/nullable? false}]}])
;               (v/validate
;                 :entity.schema.type/test-type
;                 {:test-entity/test-entity2 {:entity/instant            (Date.)
;                                             :test-entity/string-field2 "Bob"}})))))
;
;
;
;  (testing "Error Test"
;    (is (= {:test-entity/ref-field {:error/data    {:datomic-type :db.type/ref
;                                                    :valid-types  #{clojure.lang.Keyword
;                                                                    java.util.Map}
;                                                    :value        "Bob"
;                                                    :value-type   java.lang.String}
;                                    :error/message "Incorrect Value Type"
;                                    :error/type    :error.type/incorrect-type}}
;
;           (-> (create-db [{:db/id                (d/tempid :db.part/user)
;                            :entity.schema/type   {:db/id    (d/tempid :db.part/user)
;                                                   :db/ident :entity.schema.type/test-type}
;                            :entity.schema/fields [{:db/id                    (d/tempid :db.part/user)
;                                                    :field/schema             :test-entity/ref-field
;                                                    :field/entity-schema-type {:db/id    (d/tempid :db.part/user -1)
;                                                                               :db/ident :entity.schema.type/test-type2}
;                                                    :field/nullable?          false}]}
;                           {:db/id                (d/tempid :db.part/user)
;                            :entity.schema/type   (d/tempid :db.part/user -1)
;                            :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
;                                                    :field/schema    :test-entity/string-field2
;                                                    :field/nullable? false}]}])
;               (v/validate
;
;                 :entity.schema.type/test-type
;                 {:entity/schema         :entity.schema/test-entity
;                  :test-entity/ref-field "Bob"})))))
;
;  (testing "Nested Error Test"
;    (is (= {:test-entity/ref-field {:test-entity/string-field
;                                    {:error/data    {:datomic-type :db.type/string
;                                                     :valid-types  #{java.lang.String}
;                                                     :value        10.0
;                                                     :value-type   java.lang.Double}
;                                     :error/message "Incorrect Value Type"
;                                     :error/type    :error.type/incorrect-type}}}
;
;           (-> (create-db
;                 [{:db/id                (d/tempid :db.part/user)
;                   :entity.schema/type   {:db/id    (d/tempid :db.part/user)
;                                          :db/ident :entity.schema.type/test-type}
;                   :entity.schema/fields [{:db/id                    (d/tempid :db.part/user)
;                                           :field/schema             :test-entity/ref-field
;                                           :field/entity-schema-type {:db/id    (d/tempid :db.part/user -1)
;                                                                      :db/ident :entity.schema.type/test-type2}
;                                           :field/nullable?          false}]}
;                  {:db/id                (d/tempid :db.part/user)
;                   :entity.schema/type   (d/tempid :db.part/user -1)
;                   :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
;                                           :field/schema    :test-entity/string-field
;                                           :field/nullable? false}]}])
;               (v/validate
;                 :entity.schema.type/test-type
;                 {:test-entity/ref-field {:entity/instant           (Date.)
;                                          :entity/schema            :entity.schema/test-entity2
;                                          :test-entity/string-field 10.0}})))))
;
;  (testing "Test Cardinality Many"
;    (is (= {:test-entity/string-many-field #{"Bob" "Ted"}}
;
;           (-> (create-db [{:db/id                (d/tempid :db.part/user)
;                            :entity.schema/type   {:db/id    (d/tempid :db.part/user)
;                                                   :db/ident :entity.schema.type/test-type}
;                            :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
;                                                    :field/schema    :test-entity/string-many-field
;                                                    :field/nullable? false}]}])
;               (v/validate
;                 :entity.schema.type/test-type
;                 {:test-entity/string-many-field #{"Bob" "Ted"}})))))
;
;  (testing "Test Cardinality Many Ref"
;    (is (= {:test-entity/ref-many-field #{{:test-entity/string-field "Bob"}
;                                          {:test-entity/string-field "Ted"}}}
;
;           (-> (create-db [{:db/id                (d/tempid :db.part/user)
;                            :entity.schema/type   {:db/id    (d/tempid :db.part/user)
;                                                   :db/ident :entity.schema.type/test-type}
;                            :entity.schema/fields [{:db/id                    (d/tempid :db.part/user)
;                                                    :field/schema             :test-entity/ref-many-field
;                                                    :field/entity-schema-type {:db/id    (d/tempid :db.part/user -1)
;                                                                               :db/ident :entity.schema.type/test-type2}
;                                                    :field/nullable?          false}]}
;                           {:db/id                (d/tempid :db.part/user)
;                            :entity.schema/type   (d/tempid :db.part/user -1)
;                            :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
;                                                    :field/schema    :test-entity/string-field
;                                                    :field/nullable? false}]}])
;               (v/validate
;                 :entity.schema.type/test-type
;                 {:test-entity/ref-many-field #{{:entity/instant           (Date.)
;                                                 :entity/schema            :entity.schema/test-entity2
;                                                 :test-entity/string-field "Bob"}
;                                                {:entity/instant           (Date.)
;                                                 :entity/schema            :entity.schema/test-entity2
;                                                 :test-entity/string-field "Ted"}}})))))
;
;  (testing "Test Cardinality Many With Error"
;    (is (= {:test-entity/ref-many-field #{{:test-entity/string-field "Bob"}
;                                          {:test-entity/string-field {:error/data    {:datomic-type :db.type/string
;                                                                                      :valid-types  #{java.lang.String}
;                                                                                      :value        10.0
;                                                                                      :value-type   java.lang.Double}
;                                                                      :error/message "Incorrect Value Type"
;                                                                      :error/type    :error.type/incorrect-type}}}}
;
;           (-> (create-db [{:db/id                (d/tempid :db.part/user)
;                            :entity.schema/type   {:db/id    (d/tempid :db.part/user)
;                                                   :db/ident :entity.schema.type/test-type}
;                            :entity.schema/fields [{:db/id                    (d/tempid :db.part/user)
;                                                    :field/schema             :test-entity/ref-many-field
;                                                    :field/entity-schema-type {:db/id    (d/tempid :db.part/user -1)
;                                                                               :db/ident :entity.schema.type/test-type2}
;                                                    :field/nullable?          false}]}
;                           {:db/id                (d/tempid :db.part/user)
;                            :entity.schema/type   (d/tempid :db.part/user -1)
;                            :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
;                                                    :field/schema    :test-entity/string-field
;                                                    :field/nullable? false}]}])
;               (v/validate
;                 :entity.schema.type/test-type
;                 {:test-entity/ref-many-field #{{:entity/instant           (Date.)
;                                                 :entity/schema            :entity.schema/test-entity2
;                                                 :test-entity/string-field "Bob"}
;                                                {:entity/schema            :entity.schema/test-entity2
;                                                 :entity/instant           (Date.)
;                                                 :test-entity/string-field 10.0}}})))))
;
;  (testing "Pass in single value for card many field just transforms to set"
;    (is (= {:test-entity/string-many-field #{"Bob"}}
;
;           (-> (create-db [{:db/id                (d/tempid :db.part/user)
;                            :entity.schema/type   {:db/id    (d/tempid :db.part/user)
;                                                   :db/ident :entity.schema.type/test-type}
;                            :entity.schema/fields [{:db/id           (d/tempid :db.part/user)
;                                                    :field/schema    :test-entity/string-many-field
;                                                    :field/nullable? false}]}])
;               (v/validate
;                 :entity.schema.type/test-type
;                 {:test-entity/string-many-field "Bob"})))))
;
;  (testing "Test matching multiple versons of one shcema"
;    (is (= {:test-entity/string-field "Bob"}
;
;           (-> (create-db [{:db/id                  (d/tempid :db.part/user)
;
;                            :entity.schema/type     {:db/id    (d/tempid :db.part/user -1)
;                                                     :db/ident :entity.schema.type/test-type}
;                            :entity.schema/sub-type {:db/id    (d/tempid :db.part/user)
;                                                     :db/ident :entity.type/test-type}
;                            :entity.schema/fields   [{:db/id           (d/tempid :db.part/user)
;                                                      :field/schema    :test-entity/string-field
;                                                      :field/nullable? false}]}
;
;                           {:db/id                  (d/tempid :db.part/user)
;
;                            :entity.schema/type     {:db/id    (d/tempid :db.part/user -1)
;                                                     :db/ident :entity.schema.type/test-type}
;
;                            :entity.schema/sub-type {:db/id    (d/tempid :db.part/user)
;                                                     :db/ident :entity.type/test-type2}
;
;                            :entity.schema/fields   [{:db/id           (d/tempid :db.part/user)
;                                                      :field/schema    :test-entity/string-field
;                                                      :field/nullable? false}
;                                                     {:db/id           (d/tempid :db.part/user)
;                                                      :field/schema    :test-entity/string-field2
;                                                      :field/nullable? false}]}])
;               (v/validate
;                 :entity.schema.type/test-type
;                 {:entity.schema/sub-type   :entity.type/test-type
;                  :test-entity/string-field "Bob"})))))
;
;  (testing "Test Error on not suppying enough info to resolve schema"
;    (is (thrown-with-msg? AssertionError #"Assert failed: There is more than one schema of type"
;                          (= {:test-entity/string-field "Bob"}
;
;                             (-> (create-db [{:db/id                  (d/tempid :db.part/user)
;
;                                              :entity.schema/type     {:db/id    (d/tempid :db.part/user -1)
;                                                                       :db/ident :entity.schema.type/test-type}
;                                              :entity.schema/sub-type {:db/id    (d/tempid :db.part/user)
;                                                                       :db/ident :entity.type/test-type}
;                                              :entity.schema/fields   [{:db/id           (d/tempid :db.part/user)
;                                                                        :field/schema    :test-entity/string-field
;                                                                        :field/nullable? false}]}
;
;                                             {:db/id                  (d/tempid :db.part/user)
;
;                                              :entity.schema/type     {:db/id    (d/tempid :db.part/user -1)
;                                                                       :db/ident :entity.schema.type/test-type}
;
;                                              :entity.schema/sub-type {:db/id    (d/tempid :db.part/user)
;                                                                       :db/ident :entity.type/test-type2}
;
;                                              :entity.schema/fields   [{:db/id           (d/tempid :db.part/user)
;                                                                        :field/schema    :test-entity/string-field
;                                                                        :field/nullable? false}
;                                                                       {:db/id           (d/tempid :db.part/user)
;                                                                        :field/schema    :test-entity/string-field2
;                                                                        :field/nullable? false}]}])
;                                 (v/validate
;                                   :entity.schema.type/test-type
;                                   {:test-entity/string-field "Bob"})))))))






