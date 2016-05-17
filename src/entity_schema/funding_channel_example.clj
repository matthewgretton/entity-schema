(ns entity-schema.funding-channel_example
  (:require [entity-schema.yaml-conversion :as fy]
            [datomic.api :as d]
            [entity-schema.validation :as v]
            [entity-schema.entity-schema :as es]
            [entity-schema.datomic-helper :as dh])
  (:import (java.util Date UUID)))

;; set up Datomic database and get a database connection
(def uri (dh/create-in-mem-db-uri "fc-entity-db"))

(def conn (do (d/create-database uri)
              (d/connect uri)))

;; boot-strap entity schema fields
@(d/transact conn es/entity-schema-fields)

;;create yaml data structure from concentration_limit_dim.yml file
(def cl-data (fy/read-yaml-from-class-path "dimensions/concentration_limit_dim.yml"))

;;convert yaml data to field transaction data
(def cl-field-txs (fy/yaml->field-txs cl-data))

;; transact (persist) field transaction data into the database
@(d/transact conn cl-field-txs)

;; convert yaml data to entity schema transaction data
(def cl-schema-tx (fy/yaml->entity-schema-tx cl-data))

;; transact entity schema transaction.
;; note - d/transact takes a coll of transactions to persit.
;; note - datomc field data must exist before fields are referrd to. Hence, fields transacted before entity schema
@(d/transact conn [cl-schema-tx])

;;eligibility criteria
(def e-data (fy/read-yaml-from-class-path "dimensions/eligibility_criteria_dim.yml"))
(def e-field-txs (fy/yaml->field-txs e-data))
@(d/transact conn e-field-txs)
(def e-schema-tx (fy/yaml->entity-schema-tx e-data))
@(d/transact conn [e-schema-tx])

;;funding channel
(def fc-data (fy/read-yaml-from-class-path "dimensions/funding_channel_dim.yml"))
(def fc-field-txs (fy/yaml->field-txs fc-data))
@(d/transact conn fc-field-txs)
(def fc-schema-tx (fy/yaml->entity-schema-tx fc-data))
@(d/transact conn [fc-schema-tx])

;; (:db/ident e-entity-schema-tx) -> :entity.schema/eligibility-criterion
(es/pull-schema (d/db conn) :entity.schema.type/funding-channel)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reference Join Fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; db.type/ref fields (entity join fields) are not defined in the yaml files, so let's create and transact some

(def ref-field-txs
  [;; create a join field from function channel to elibility criteria, not it's a cardinality many field
   {:db/id                 (d/tempid :db.part/db)           ;; this creates a temporary id in the db partition
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

@(d/transact conn ref-field-txs)

;; now we have created and trasacted the ref fields (join fields) add schema field entries to the schema.
(def add-ref-fields-to-fc-schema-tx
  {:db/id                :entity.schema/funding-channel
   :entity.schema/fields [{:db/id               (d/tempid :db.part/user)
                           :field/schema        :funding-channel/concentration-limits
                           :field/entity-schema-type :entity.schema.type/concentration-limit
                           :field/nullable?     false}
                          {:db/id               (d/tempid :db.part/user)
                           :field/schema        :funding-channel/eligibility-criterions
                           :field/entity-schema-type :entity.schema.type/eligibility-criterion
                           :field/nullable?     false}]})

@(d/transact conn [add-ref-fields-to-fc-schema-tx])



;; An example of a funding channel entity with eligibility criterions and concentration limits
(def full-fc-entity
  {:entity/schema :entity.schema/funding-channel
   :entity/instant                               (Date.)
   :funding-channel/funding-channel-uuid         (UUID/randomUUID)
   :funding-channel/name                         "Dorset Rise Ltd"
   :funding-channel/referral-only?               false
   :funding-channel/scale-allocation-percentage? true
   :funding-channel/allocation-percentage        (bigdec 0.7)
   :funding-channel/eligibility-criterions       #{{:entity/instant                               (Date.)
                                                    :entity/schema :entity.schema/eligibility-criterion
                                                    :eligibility-criterion/criterion-type      "Include"
                                                    :eligibility-criterion/criterion-attribute ":risk-band"
                                                    :eligibility-criterion/criterion-value     "#{:Aplus :A :B :C :D}"}
                                                   {:entity/instant                               (Date.)
                                                    :entity/schema :entity.schema/eligibility-criterion
                                                    :eligibility-criterion/criterion-type      "Include"
                                                    :eligibility-criterion/criterion-attribute ":secured"
                                                    :eligibility-criterion/criterion-value     "#{false}"}}

   :funding-channel/concentration-limits         #{{:entity/instant                               (Date.)
                                                    :entity/schema :entity.schema/concentration-limit
                                                    :concentration-limit/threshold        (float 0.7)
                                                    :concentration-limit/constraint-type      "MaxAllocationLifetime"
                                                    :concentration-limit/constrained-attribute ":original-principal-cents"
                                                    :concentration-limit/constrained-value     "1000"}

                                                   {:entity/instant                               (Date.)
                                                    :entity/schema :entity.schema/concentration-limit
                                                    :concentration-limit/threshold (float 0.7)
                                                    :concentration-limit/constraint-type      "MaxAllocationMonthly"
                                                    :concentration-limit/constrained-attribute ":original-principal-cents"
                                                    :concentration-limit/constrained-value     "100"}}})

(->> (es/pull-schema-by-id (d/db conn) :entity.schema/funding-channel)
     (:entity.schema/fields)
     (map :field/schema)
     (map :db/ident))

(v/validate(d/db conn)  :entity.schema.type/funding-channel full-fc-entity)

(def schema
  {:db/id (d/tempid :db.part/user)
   :})

(def ref-field-txs
  [;; create a join field from function channel to elibility criteria, not it's a cardinality many field
   {:db/id                 (d/tempid :db.part/db)           ;; this creates a temporary id in the db partition
    :db.install/_attribute :db.part/db
    :db/ident              :concentration-limit/risk-bands
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/many}

   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :concentration-limit/secured?
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/true}


   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :eligibility-criterion/secured?
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/true}

   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :eligibility-criterion/original-principal-cents
    :db/valueType          :db.type/long
    :db/cardinality        :db.cardinality/true}



   ])

(def include-risk-schema
  [{:db/id (d/tempid :db.part/user)
   :db/ident :entity.schema/concetration-limit-include-risk
   :entity.schema/fields #{{:db/id (d/tempid :db.part/user)
                            :field/schema :concentration-limit/risk-bands}}}

   {:db/id (d/tempid :db.part/user)
    :db/ident :entity.schema/concetration-limit-secured
    :entity.schema/fields #{{:db/id (d/tempid :db.part/user)
                             :field/schema :concentration-limit/secured?}}}

   ])









