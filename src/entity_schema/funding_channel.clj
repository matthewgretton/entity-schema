(ns entity-schema.funding-channel
  (:require [entity-schema.from-yaml :as fy]
            [datomic.api :as d]
            [entity-schema.entity-schema :as es])
  (:import (java.util Date)))

(defn create-bootstrapped-conn []
  (let [uri "datomic:mem://entity-db"]
    (d/delete-database uri)
    (d/create-database uri)
    (let [conn (d/connect uri)]
      (es/bootstrap conn)
      conn)))


(def conn (create-bootstrapped-conn))

(fy/create-entity-schema conn "dimensions/concentration_limit_dim.yml")
;; :entity.schema/concentration-limit

(fy/create-entity-schema conn "dimensions/eligibility_criteria_dim.yml")
;; :entity.schema/eligibility-criterion

(fy/create-entity-schema conn "dimensions/funding_channel_dim.yml")
;; :entity.schema/funding-channel

(es/pull-schema (d/db conn) :entity.schema/eligibility-criterion)

(es/pull-expanded-schema (d/db conn) :entity.schema/eligibility-criterion)

(es/get-schema (d/db conn) {:event/instant (Date.)
                            :db/ident :entity.schema/eligibility-criterion})












;; not defined in the yaml
(def ref-fields
  [{:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :funding-channel/eligibility-criterions
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/many}

   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :funding-channel/concentration-limits
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/many}
   ])

@(d/transact conn ref-fields)

(def add-ref-fields
  {:db/id :entity.schema/funding-channel
   :entity.schema/fields [{:db/id (d/tempid :db.part/user)
                           :field/schema :funding-channel/concentration-limits
                           :field/nullable? false
                           :field/entity-schema :entity.schema/concentration-limit}
                          {:db/id (d/tempid :db.part/user)
                           :field/schema :funding-channel/eligibility-criterions
                           :field/entity-schema :entity.schema/eligibility-criterion
                           :field/nullable? false}]})

@(d/transact conn [add-ref-fields])
