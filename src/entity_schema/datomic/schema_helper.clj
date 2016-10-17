(ns entity-schema.datomic.schema-helper
  (:require [entity-schema.datomic.datomic-helper :as dh]
            [datomic.api :as d]))


(defn make-schema-stub-compatible-with-datomic [db {:keys [:db/ident :entity.schema/fields
                                                           :entity.schema/natural-key :entity.schema/part] :as schema}]
  (assert (dh/all-indexed? db natural-key) (str "All natural key attributes should be indexed " natural-key "\n" schema))
  {:db/id                     (d/tempid :db.part/entity-schema)
   :db/ident                  ident
   :entity.schema/part        part
   :entity.schema/fields      (->> fields
                                   (map (fn [f]
                                          (let [f-wth-id (assoc f :db/id (d/tempid :db.part/entity-schema))]
                                            (if-let [sub-schema (:field/entity-schema f-wth-id)]
                                              (assoc f-wth-id :field/entity-schema   (make-schema-stub-compatible-with-datomic db sub-schema))
                                              f-wth-id)
                                            )))
                                   (into #{}))
   :entity.schema/natural-key (dh/build-datomic-linked-list part natural-key)
   })