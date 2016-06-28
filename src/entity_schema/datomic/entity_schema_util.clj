(ns entity-schema.datomic.entity-schema-util
  (:require [entity-schema.datomic.entity-schema :as es]))

(defn pull-schema-by-type
  ([db schema-type]
   (->> (es/->entity-schema-ids db schema-type)
        (map #(es/->entity-schema db %))
        (into #{})))
  ([db schema-type entity-type]
   (->> (es/->entity-schema-ids db schema-type entity-type)
        (map #(es/->entity-schema db %))
        (into #{}))))

(defn first-of-one [coll]
  (assert (= (count coll) 1) (str "There is expected to be only one item in the coll" "\n"
                                  (with-out-str (clojure.pprint/pprint coll))))
  (first coll))

(defn derive-schema
  "Derive the schema from the entity"
  [db field entity]
  (assert (not (nil? db)))
  (let [{{schema-type :db/ident} :field/entity-schema-type} field]
    (assert (not (nil? schema-type)))
    (first-of-one
      (if-let [sub-type (es/->sub-type-id db entity)]
        (pull-schema-by-type db schema-type sub-type)
        (pull-schema-by-type db schema-type)))))