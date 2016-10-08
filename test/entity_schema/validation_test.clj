(ns entity-schema.validation-test
  (:require [clojure.test :refer :all]
            [entity-schema.datomic.schema-helper :as schema-helper]
            [datomic.api :as d]
            [entity-schema.datomic.datomic-helper :as dh]
            [entity-schema.processor :as p]
            [entity-schema.datomic.entity-schema :as es]
            [entity-schema.datomic.entity-schema-data :as esd]
            [entity-schema.util.bimap :as bimap]
            [entity-schema.util.core :as util]))

(defn transform [actual expected input-exp-act-bimap]
  (->> (util/flatten-upto-key expected :db/id)
       (reduce (fn [[exp-act-bimap output] [expected-id-path expected-id]]
                 (if-let [actual-id (get-in actual expected-id-path)]
                   (cond

                     ;; consistently unmapped, but partitions are the same
                     (and (bimap/consistently-unmapped? exp-act-bimap expected-id actual-id)
                          (= (:part actual-id) (:part expected-id)))
                     [(bimap/assoc exp-act-bimap expected-id actual-id) (assoc-in output expected-id-path expected-id)]

                     ;; consistently mapped
                     (bimap/consistently-mapped? exp-act-bimap expected-id actual-id)
                     [exp-act-bimap (assoc-in output expected-id-path expected-id)]

                     ;; there's an inconsistency between the mapping and the inputs
                     :else
                     [exp-act-bimap (->> (assoc actual-id :idx {:actual-id     actual-id
                                                                :expected-id   expected-id
                                                                :mapped-act-id (bimap/get-value exp-act-bimap expected-id)
                                                                :mapped-exp-id (bimap/get-key exp-act-bimap actual-id)})
                                         (assoc-in output expected-id-path))])
                   [exp-act-bimap output])) [input-exp-act-bimap actual])))

(defn transform-all [actuals expecteds]
  (->> (map vector actuals expecteds)
       (reduce (fn [[exp-act-id-bi-map output-coll] [actual expected]]
                 (let [[new-exp-act-id-bi-map output] (transform actual expected exp-act-id-bi-map)]
                   [new-exp-act-id-bi-map (conj output-coll output)])) [(bimap/create-empty) []])
       (second)))


(require 'spyscope.core)

(defn test-entities [test-desc fields schema command-data input-entities [expected-pairs expected-errored?]]
  (let [uri (dh/create-in-mem-db-uri "entity-db")
        conn (do (d/create-database uri) (d/connect uri))]
    @(d/transact conn (->> fields (map #(dh/create-field %)) (into [])))
    (let [db (d/db conn)
          datomic-compat-schema (schema-helper/make-compatible-with-datomic db schema)]
      @(d/transact conn esd/all-fields)
      @(d/transact conn [datomic-compat-schema])
      (let [pulled-schema (es/pull-entity-schema (d/db conn) (:db/ident datomic-compat-schema))]
        (let [[actual-pairs actual-errored?] (p/process-all-entities db pulled-schema command-data input-entities)]
          (d/delete-database uri)
          (let [actual-entities (map first actual-pairs)
                expected-entities (map first expected-pairs)
                actual-errors (map second actual-pairs)
                transformed-actual-entities (transform-all actual-entities expected-entities)
                transformed-actual-pairs (->> (map vector transformed-actual-entities actual-errors) (into []))]
            (is (= [transformed-actual-pairs actual-errored?]
                   [expected-pairs expected-errored?]) test-desc)))))))

;;test method
(defn test-valid-single-entity [test-desc fields schema command-data input-entity expected-ouput]
  (test-entities test-desc fields schema command-data [input-entity] [[[expected-ouput false]] false]))

(defn test-invalid-single-entity [test-desc fields schema command-data input-entity expected-ouput]
  (test-entities test-desc fields schema command-data [input-entity] [[[expected-ouput true]] true]))

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


(deftest multiple-entity-validation-tests
  (test-entities "Valid"
                 [{:db/ident       :test-entity/string-field
                   :db/valueType   :db.type/string
                   :db/cardinality :db.cardinality/one
                   :db/unique      :db.unique/identity}]

                 {:db/ident                  :entity.schema/test
                  :entity.schema/part        :db.part/entity-schema
                  :entity.schema/fields      [{:field/nullable? true
                                               :field/schema    :test-entity/string-field}]
                  :entity.schema/natural-key [:test-entity/string-field]}

                 insert-command-map
                 [{:test-entity/string-field "Bob12"}
                  {:test-entity/string-field "Bob12"}]
                 [[[{:test-entity/string-field "Bob12"
                     :db/id                    (d/tempid :db.part/entity-schema -1)} false]
                   [{:test-entity/string-field "Bob12"
                     :db/id                    (d/tempid :db.part/entity-schema -1)} false]
                   ] false])

  (test-entities "Valid"
                 [string-field] simple-schema-required-field insert-command-map
                 [{:test-entity/string-field "Bob12"}]
                 [[[{:test-entity/string-field "Bob12"
                     :db/id                    (d/tempid :db.part/entity-schema)} false]] false]))




(deftest single-entity-validation-tests

  (test-valid-single-entity "Valid"
                            [string-field] simple-schema-required-field insert-command-map
                            {:test-entity/string-field "Bob12"}
                            {:db/id                    (d/tempid :db.part/entity-schema)
                             :test-entity/string-field "Bob12"})

  (test-invalid-single-entity "Incorrect field type"
                            [string-field] simple-schema-required-field insert-command-map
                            {:test-entity/string-field 12}
                            {:test-entity/string-field {:error/data    {:datomic-type :db.type/string
                                                                        :valid-types  #{java.lang.String}
                                                                        :value        12
                                                                        :value-type   java.lang.Long}
                                                        :error/message "Incorrect Value Type"
                                                        :error/type    :error.type/incorrect-type}})

  (test-invalid-single-entity "Missing required field"
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
  (test-invalid-single-entity "Missing (optional) natural key"
                            [string-field] simple-schema-optional-field insert-command-map
                            {}
                            {:db/id {:error/message "Missing natural key"
                                     :error/type    :error.type/missing-natrual-key
                                     :error/data    {:error/entity      {}
                                                     :error/natural-key [:test-entity/string-field]}}})

  (test-invalid-single-entity "Cardinality Test"
                            [string-field] {:db/ident                  :entity.schema/test
                                            :entity.schema/part        :db.part/entity-schema
                                            :entity.schema/fields      [{:field/nullable? true
                                                                         :field/schema    :test-entity/string-field}]
                                            :entity.schema/natural-key [:test-entity/string-field]} insert-command-map
                            {:test-entity/string-field ["Bob" "Ted"]}
                            {:test-entity/string-field {:error/data    {:value ["Bob"
                                                                                "Ted"]}
                                                        :error/message "Value associated with cardinality one field should not be a collection"
                                                        :error/type    :error.type/cardinality}})

  (test-valid-single-entity "Single Value input for Cardinality many field should be fine"
                            [{:db/ident       :test-entity/string-field
                              :db/valueType   :db.type/string
                              :db/cardinality :db.cardinality/many
                              :db/unique      :db.unique/identity}]
                            {:db/ident                  :entity.schema/test
                             :entity.schema/part        :db.part/entity-schema
                             :entity.schema/fields      [{:field/nullable? true
                                                          :field/schema    :test-entity/string-field}]
                             :entity.schema/natural-key [:test-entity/string-field]} insert-command-map
                            {:test-entity/string-field "Bob"}
                            {:db/id (d/tempid :db.part/entity-schema) 
                             :test-entity/string-field #{"Bob"}})

  ;(test-single-entity "Test expanding"
  ;                    [{:db/ident       :test-entity/ref-field
  ;                      :db/valueType   :db.type/ref
  ;                      :db/cardinality :db.cardinality/one
  ;                      :db/unique      :db.unique/identity}]
  ;                    {:db/ident                  :entity.schema/test
  ;                     :entity.schema/part        :db.part/entity-schema
  ;                     :entity.schema/fields      [{:field/nullable?     true
  ;                                                  :field/schema        :test-entity/ref-field
  ;                                                  :field/entity-schema {}}]
  ;                     :entity.schema/natural-key [:test-entity/string-field]} insert-command-map
  ;                    {:test-entity/string-field "Bob"}
  ;                    {:test-entity/string-field #{"Bob"}})

  )


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






