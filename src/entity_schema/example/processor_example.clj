(ns entity-schema.example.processor-example
  (:require [entity-schema.yaml-conversion :as fy]
            [datomic.api :as d]
            [entity-schema.datomic-helper :as dh]
            [entity-schema.entity-schema :as es]
            [entity-schema.validation :as v]
            [entity-schema.processor :as p]
            [clojure.core.reducers :as r])
  (:import (java.util UUID Date)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This example creates some schema from yaml files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO Put some time stamps in so different lines can be re-run out of order

;; set up Datomic database and get a database connection
(def uri (dh/create-in-mem-db-uri "fc-entity-db"))

(def conn (do (d/create-database uri)
              (d/connect uri)))

;; boot-strap entity schema fields
@(d/transact conn es/all-fields)

;;funding channel

;;create clojure data structure from yaml file
(def fc-data (fy/read-yaml-from-class-path "dimensions/funding_channel_dim.yml"))

;;convert yaml data to field transaction data
(def fc-field-txs (fy/yaml->field-txs fc-data))

;; transact (persist) field transaction data into the database
@(d/transact conn fc-field-txs)

;; convert yaml data to entity schema transaction data
(def fc-schema-tx (fy/yaml->entity-schema-tx fc-data))


;; note that datomc fields must have been transacted before they are referrd to.
;; Hence, fields transacted before entity schema
@(d/transact conn [fc-schema-tx])



;;eligibility criteria
(def e-data (fy/read-yaml-from-class-path "dimensions/eligibility_criteria_dim.yml"))
(def e-field-txs (fy/yaml->field-txs e-data))
@(d/transact conn e-field-txs)
(def e-schema-tx (fy/yaml->entity-schema-tx e-data))
@(d/transact conn [e-schema-tx])


;; immutable database snapshot
(def first-draft-schema-db (d/db conn))


;; Let's take a look at the schema we've created. We pull all schema by there type
;; (d/db conn) gives an immutable shapshot of the database at the moment it is called
(es/pull-schema-by-type first-draft-schema-db :entity.schema.type/funding-channel)
(es/pull-schema-by-type first-draft-schema-db :entity.schema.type/eligibility-criterion)


;; Great, so we've created 3 schema, but there is nothing linking those things together in the YAML
;; Let's add some join fields (:db.type/ref)


(def join-field-txs
  [;; create a join field from function channel to elibility criteria, not it's a cardinality many field
   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :funding-channel/eligibility-criterions
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/many}            ;; one to many join
   ])

@(d/transact conn join-field-txs)

;; now we have created and trasacted the join fields, we also need to add references in the entity schema
(def add-ref-fields-to-funding-channel-schema-tx
  {:db/id :entity.schema/funding-channel                    ;; we created the schema with a db/ident field so they can easily referened
   :entity.schema/fields
          [{:db/id                    (d/tempid :db.part/user)
            :field/schema             :funding-channel/eligibility-criterions
            :field/entity-schema-type :entity.schema.type/eligibility-criterion
            :field/nullable?          false}]})

@(d/transact conn [add-ref-fields-to-funding-channel-schema-tx])


(def schema-with-joins-db (d/db conn))

;;Let's take a looka the full entity-schema for the funding cicle with joins
(es/pull-schema-by-type schema-with-joins-db :entity.schema.type/funding-channel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nested validation example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ok so we created 3 entity-schema from the YAML files, and  added join fields between them. Let's create an
;; example entity and try to validate it.


;; An example of a funding channel entity with eligibility criterions and concentration limits
(def full-fc-entity
  {:entity/instant                               (Date.)
   :funding-channel/uuid                         (UUID/randomUUID)
   :funding-channel/name                         "Dorset Rise Ltd"
   :funding-channel/referral-only?               false
   :funding-channel/scale-allocation-percentage? true
   :funding-channel/allocation-percentage        (bigdec 0.7)
   :funding-channel/eligibility-criterions       #{{:eligibility-criterion/criterion-type      "Include"
                                                    :eligibility-criterion/criterion-attribute ":risk-band"
                                                    :eligibility-criterion/criterion-value     "#{:Aplus :A :B :C :D}"}

                                                   {:eligibility-criterion/criterion-type      "Include"
                                                    :eligibility-criterion/criterion-attribute ":secured"
                                                    :eligibility-criterion/criterion-value     "#{false}"}}})


(def ent2
  (assoc full-fc-entity :funding-channel/uuid (UUID/randomUUID)))


(p/process (d/db conn) :entity.schema.type/funding-channel
           {} :command/insert
           full-fc-entity)




;;Can we make the combine method associtive?????
(def entities (->> (p/process-all (d/db conn) :entity.schema.type/funding-channel
                    {} :command/insert
                    [full-fc-entity
                     ent2
                     ent2
                     full-fc-entity
                     ])
     (into [])))

@(d/transact conn entities)


(def entities2 (->> (p/process-all (d/db conn) :entity.schema.type/funding-channel
                                  {} :command/update
                                  [full-fc-entity
                                   ent2
                                   ent2
                                   full-fc-entity
                                   ])
                   (into [])))

@(d/transact conn entities2)



