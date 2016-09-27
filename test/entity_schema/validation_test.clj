(ns entity-schema.validation-test
  (:require [clojure.test :refer :all]
            [entity-schema.datomic.schema-helper :as schema-helper]
            [datomic.api :as d]
            [entity-schema.datomic.datomic-helper :as dh]
            [entity-schema.processor :as p]
            [entity-schema.datomic.entity-schema :as es]
            [entity-schema.datomic.entity-schema-data :as esd]
            [entity-schema.validation :as v]))

;;test method
(defn test-single-entity [test-desc fields schema command-data input-entity expected-ouput]
  (let [uri (dh/create-in-mem-db-uri "entity-db")
        conn (do (d/create-database uri) (d/connect uri))]
    @(d/transact conn (->> fields (map #(dh/create-field %)) (into [])))

    (let [db (d/db conn)
          datomic-compat-schema (schema-helper/make-compatible-with-datomic db schema)]
      @(d/transact conn esd/all-fields)
      @(d/transact conn [datomic-compat-schema])
      (let [pulled-schema (es/pull-entity-schema (d/db conn) (:db/ident datomic-compat-schema))
            [pairs errored?] (p/process-all-entities db pulled-schema command-data [input-entity])
            [result res-errored?] (first pairs)
            id (:db/id result)
            final-result (if (v/error? id) result (dissoc result :db/id))]
        (d/delete-database uri)
        (if (and (contains? result :db/id) (not (v/error? id)))
          (is (= (:part id) (:entity.schema/part schema)) test-desc))
        (is (= errored? res-errored?) test-desc)
        (is (= expected-ouput final-result) test-desc)))))

(= (d/tempid :db.part/entity-schema -4) (d/tempid :db.part/entity-schema -4))

(def string-field
  {:db/ident       :test-entity/string-field
   :db/valueType   :db.type/string
   :db/cardinality :db.cardinality/one
   :db/unique      :db.unique/identity})

(def simple-schema-required-field
  {:db/ident                  :entity.schema/test
   :entity.schema/part        :db.part/entity-schema
   :entity.schema/fields      [{:field/nullable? false
                                :field/schema    :test-entity/string-field}]
   :entity.schema/natural-key [:test-entity/string-field]})

(def simple-schema-optional-field
  {:db/ident                  :entity.schema/test
   :entity.schema/part        :db.part/entity-schema
   :entity.schema/fields      [{:field/nullable? true
                                :field/schema    :test-entity/string-field}]
   :entity.schema/natural-key [:test-entity/string-field]})

(def insert-command-map
  {:command-map     {:entity.schema/test :command/insert}
   :default-command :command/look-up})

(deftest single-entity-validation-test

  (test-single-entity "Valid"
                      [string-field] simple-schema-required-field insert-command-map
                      {:test-entity/string-field "Bob12"}
                      {:test-entity/string-field "Bob12"})

  (test-single-entity "Incorrect field type"
                      [string-field] simple-schema-required-field insert-command-map
                      {:test-entity/string-field 12}
                      {:test-entity/string-field {:error/data    {:datomic-type :db.type/string
                                                                  :valid-types  #{java.lang.String}
                                                                  :value        12
                                                                  :value-type   java.lang.Long}
                                                  :error/message "Incorrect Value Type"
                                                  :error/type    :error.type/incorrect-type}})

  (test-single-entity "Missing required field"
                      [string-field] simple-schema-required-field insert-command-map
                      {}
                      {:test-entity/string-field {:error/message "Required Field"
                                                  :error/type    :error.type/required-field}})

  ;(test-single-entity "Function test"
  ;                    [string-field] simple-schema-optional-field insert-command-map
  ;                    {}
  ;                    {:test-entity/string-field {:error/message "Required Field"
  ;                                                :error/type    :error.type/required-field}})

  ;; What do we need to happen here?
  (test-single-entity "Missing (optional) natural key"
                      [string-field] simple-schema-optional-field insert-command-map
                      {}
                      {:db/id {:error/message "Missing natural key"
                               :error/type    :error.type/missing-natrual-key
                               :error/data    {:error/entity      {}
                                               :error/natural-key [:test-entity/string-field]}}}))





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
