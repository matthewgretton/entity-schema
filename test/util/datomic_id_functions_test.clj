(ns util.datomic-id-functions-test
  "Tests to show how the make-ids-consistent function works"
  (:require [util.datomic_id_function :as id-func]
            [clojure.test :refer :all]
            [datomic.api :as d]))


(defn create-comparable-output [desc actual expected expected-output]
  (let [actual-made-consistent (id-func/make-ids-consistent actual expected)]
    [expected-output actual-made-consistent desc]))

(deftest making-ids-consistent-tests

  (->> (create-comparable-output "Ids are equivalent"

                                 [{:db/id (d/tempid :db.part/user -2)}
                                  {:db/id (d/tempid :db.part/user -2)}]

                                 [{:db/id (d/tempid :db.part/user -1)}
                                  {:db/id (d/tempid :db.part/user -1)}]


                                 [{:db/id (d/tempid :db.part/user -1)}
                                  {:db/id (d/tempid :db.part/user -1)}])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (create-comparable-output "Entity with stuff"
                                 [{:test-entity/string-field "Bob12",
                                   :db/id                    {:part :db.part/entity-schema, :idx -1000104}}
                                  {:test-entity/string-field "Bob12",
                                   :db/id                    {:part :db.part/entity-schema, :idx -1000104}}]
                                 [{:test-entity/string-field "Bob12",
                                   :db/id                    {:part :db.part/entity-schema, :idx -1}}
                                  {:test-entity/string-field "Bob12",
                                   :db/id                    {:part :db.part/entity-schema, :idx -1}}]

                                 [{:db/id                    {:part :db.part/entity-schema, :idx -1}
                                   :test-entity/string-field "Bob12"}
                                  {:db/id                    {:part :db.part/entity-schema, :idx -1}
                                   :test-entity/string-field "Bob12"}])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (create-comparable-output "Nested Ids are equivalent"

                                 [{:db/id      (d/tempid :db.part/user -3)
                                   :entity/ref {:db/id (d/tempid :db.part/user -4)}}]

                                 [{:db/id      (d/tempid :db.part/user -1)
                                   :entity/ref {:db/id (d/tempid :db.part/user -2)}}]



                                 [{:db/id      (d/tempid :db.part/user -1)
                                   :entity/ref {:db/id (d/tempid :db.part/user -2)}}])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (create-comparable-output "Nested equivalent ids with same ref"
                                 [{:db/id      (d/tempid :db.part/user -3)
                                   :entity/ref {:db/id (d/tempid :db.part/user -2)}}]

                                 [{:db/id      (d/tempid :db.part/user -1)
                                   :entity/ref {:db/id (d/tempid :db.part/user -2)}}]

                                 [{:db/id      (d/tempid :db.part/user -1)
                                   :entity/ref {:db/id (d/tempid :db.part/user -2)}}])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (create-comparable-output "Nested equivalent transactions with transacted id"
                                 [{:db/id      (d/tempid :db.part/user -3)
                                   :entity/ref {:db/id :some-ident}}]

                                 [{:db/id      (d/tempid :db.part/user -1)
                                   :entity/ref {:db/id :some-ident}}]

                                 [{:db/id      (d/tempid :db.part/user -1)
                                   :entity/ref {:db/id :some-ident}}])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (create-comparable-output "Missing Id"
                                 [{}]

                                 [{:db/id (d/tempid :db.part/user -1)}]

                                 [{:db/id {:error/type :error.type/expected-id,
                                           :error/data {:expected-id (d/tempid :db.part/user -1)}}}])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (create-comparable-output "Expected and actual have inconsistent partitions"

                                 [{:db/id (d/tempid :db.part/user -2)}
                                  {:db/id (d/tempid :db.part/user -2)}]

                                 [{:db/id (d/tempid :db.part/user -1)}
                                  {:db/id (d/tempid :db.part/custom -1)}]

                                 [{:db/id (d/tempid :db.part/user -1)}
                                  {:db/id {:error/data {:actual-id     (d/tempid :db.part/user -2)
                                                        :expected-id   (d/tempid :db.part/custom -1)
                                                        :mapped-exp-id (d/tempid :db.part/user -1)}
                                           :error/type :error.type/inconsisten-ids}}])
       ((fn [[exp act desc]] (is (= exp act) desc))))


  (->> (create-comparable-output "Expected ids different, actual the same"
                                 [{:db/id (d/tempid :db.part/user -2)}
                                  {:db/id (d/tempid :db.part/user -2)}]

                                 [{:db/id (d/tempid :db.part/user -1)}
                                  {:db/id (d/tempid :db.part/user -3)}]

                                 [{:db/id (d/tempid :db.part/user -1)}
                                  {:db/id {:error/data {:actual-id     (d/tempid :db.part/user -2)
                                                        :expected-id   (d/tempid :db.part/user -3)
                                                        :mapped-exp-id (d/tempid :db.part/user -1)}
                                           :error/type :error.type/inconsisten-ids}}])
       ((fn [[exp act desc]] (is (= exp act) desc))))


  (->> (create-comparable-output "Expected ids the same, actual different"
                                 [{:db/id (d/tempid :db.part/user -4)}
                                  {:db/id (d/tempid :db.part/user -2)}]

                                 [{:db/id (d/tempid :db.part/user -1)}
                                  {:db/id (d/tempid :db.part/user -1)}]

                                 [{:db/id (d/tempid :db.part/user -1)}
                                  {:db/id {:error/data {:actual-id     (d/tempid :db.part/user -2)
                                                        :expected-id   (d/tempid :db.part/user -1)
                                                        :mapped-act-id (d/tempid :db.part/user -4)}
                                           :error/type :error.type/inconsisten-ids}}])
       ((fn [[exp act desc]] (is (= exp act) desc))))


  (->> (create-comparable-output "Different transacted ids"

                                 [{:db/id :something2}]

                                 [{:db/id :something}]

                                 [{:db/id {:error/type :error.type/inconsisten-ids
                                           :error/data {:actual-id   :something2
                                                        :expected-id :something}}}])
       ((fn [[exp act desc]] (is (= exp act) desc))))


  (->> (create-comparable-output "When the expected id has an ident and that is used to refer"

                                 [{:db/id 123456}]

                                 [{:db/id :something}]

                                 [{:db/id :something}])
       ((fn [[exp act desc]] (is (= exp act) desc))))

  (->> (create-comparable-output "When the expected id has an ident and that is used to refer"

                                 [{:db/id 123456}
                                  {:db/id 123456}]

                                 [{:db/id :something}
                                  {:db/id :something-else}]

                                 [{:db/id :something}
                                  {:db/id {:error/data {:actual-id     123456
                                                        :expected-id   :something-else
                                                        :mapped-exp-id :something}
                                           :error/type :error.type/inconsisten-ids}}])
       ((fn [[exp act desc]] (is (= exp act) desc))))



  )




