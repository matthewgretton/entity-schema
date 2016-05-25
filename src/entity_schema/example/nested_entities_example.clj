(ns entity-schema.example.nested-entities-example
  (:require [entity-schema.yaml-conversion :as fy]
            [datomic.api :as d]
            [entity-schema.datomic-helper :as dh]
            [entity-schema.entity-schema :as es]
            [entity-schema.validation :as v])
  (:import (java.util UUID Date)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This example creates some schema from yaml files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set up Datomic database and get a database connection
(def uri (dh/create-in-mem-db-uri "fc-entity-db"))

(def conn (do (d/create-database uri)
              (d/connect uri)))

;; boot-strap entity schema fields
@(d/transact conn es/entity-schema-fields)

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



;;concentration limit
(def cl-data (fy/read-yaml-from-class-path "dimensions/concentration_limit_dim.yml"))
(def cl-field-txs (fy/yaml->field-txs cl-data))
@(d/transact conn cl-field-txs)
(def cl-schema-tx (fy/yaml->entity-schema-tx cl-data))
@(d/transact conn [cl-schema-tx])

;;eligibility criteria
(def e-data (fy/read-yaml-from-class-path "dimensions/eligibility_criteria_dim.yml"))
(def e-field-txs (fy/yaml->field-txs e-data))
@(d/transact conn e-field-txs)
(def e-schema-tx (fy/yaml->entity-schema-tx e-data))
@(d/transact conn [e-schema-tx])


;; Let's take a look at the schema we've created. We pull all schema by there type
;; (d/db conn) gives an immutable shapshot of the database at the moment it is called
(es/pull-schema-by-type (d/db conn) :entity.schema.type/funding-channel)
(es/pull-schema-by-type (d/db conn) :entity.schema.type/concentration-limit)
(es/pull-schema-by-type (d/db conn) :entity.schema.type/eligibility-criterion)


;; Great, so we've created 3 schema, but there is nothing linking those things together in the YAML
;; Let's add some join fields (:db.type/ref)


(def join-field-txs
  [;; create a join field from function channel to elibility criteria, not it's a cardinality many field
   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :funding-channel/eligibility-criterions
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/many}            ;; one to many join

   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :funding-channel/concentration-limits
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/many}            ;; one to many join
   ])

@(d/transact conn join-field-txs)

;; now we have created and trasacted the join fields, we also need to add references in the entity schema
(def add-ref-fields-to-funding-channel-schema-tx
  {:db/id :entity.schema/funding-channel                    ;; we created the schema with a db/ident field so they can easily referened
   :entity.schema/fields
          [{:db/id                    (d/tempid :db.part/user)
            :field/schema             :funding-channel/concentration-limits
            :field/entity-schema-type :entity.schema.type/concentration-limit
            :field/nullable?          false}
           {:db/id                    (d/tempid :db.part/user)
            :field/schema             :funding-channel/eligibility-criterions
            :field/entity-schema-type :entity.schema.type/eligibility-criterion
            :field/nullable?          false}]})

@(d/transact conn [add-ref-fields-to-funding-channel-schema-tx])

;;Let's take a looka the full entity-schema for the funding cicle with joins
(es/pull-schema-by-type (d/db conn) :entity.schema.type/funding-channel)




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
                                                    :eligibility-criterion/criterion-value     "#{false}"}}

   :funding-channel/concentration-limits         #{{:concentration-limit/threshold             (float 0.7)
                                                    :concentration-limit/constraint-type       "MaxAllocationLifetime"
                                                    :concentration-limit/constrained-attribute ":original-principal-cents"
                                                    :concentration-limit/constrained-value     "1000"}

                                                   {:concentration-limit/threshold             (float 0.7)
                                                    :concentration-limit/constraint-type       "MaxAllocationMonthly"
                                                    :concentration-limit/constrained-attribute ":original-principal-cents"
                                                    :concentration-limit/constrained-value     "100"}}})



(v/validate (d/db conn) :entity.schema.type/funding-channel full-fc-entity)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The reason the  concentration limits (and elibility-criterion) are modelled in this general way, is that there are
;; in fact many types of concentratoin limit. The two examples in the entity above clearly demonstrate this. Ideally,
;; we would want to model these as different entity types. We can do this as follows:

;; create new fields and new schema:

;
(def risk-band-enum-values
  [{:db/id    (d/tempid :db.part/user)
    :db/ident :risk-band/Aplus}
   {:db/id    (d/tempid :db.part/user)
    :db/ident :risk-band/A}
   {:db/id    (d/tempid :db.part/user)
    :db/ident :risk-band/B}
   {:db/id    (d/tempid :db.part/user)
    :db/ident :risk-band/C}
   {:db/id    (d/tempid :db.part/user)
    :db/ident :risk-band/D}
   ])

(def eligibility-criterion-fields
  [;; create a join field from function channel to elibility criteria, not it's a cardinality many field

   {:db/id                 (d/tempid :db.part/db)           ;; this creates a temporary id in the db partition
    :db.install/_attribute :db.part/db
    :db/ident              :eligibility-criterion/risk-bands
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/many}

   {:db/id                 (d/tempid :db.part/db)
    :db.install/_attribute :db.part/db
    :db/ident              :eligibility-criterion/secured?
    :db/valueType          :db.type/boolean
    :db/cardinality        :db.cardinality/one}])

@(d/transact conn (concat eligibility-criterion-fields
                          risk-band-enum-values))

(def include-eligiblity-schema
  [
   {:db/id    (d/tempid :db.part/user -1)
    :db/ident :entity.schema.type/risk-band}

   {:db/id                (d/tempid :db.part/user)
    :db/ident             :entity.schema/risk-band
    :entity.schema/type   (d/tempid :db.part/user -1)
    :entity.schema/fields #{{:db/id           (d/tempid :db.part/user)
                             :field/schema    :db/ident
                             :field/nullable? false}}}

   {:db/id                  (d/tempid :db.part/user)
    :db/ident               :entity.schema/eligibility-criterion-include-risk
    :entity.schema/type     :entity.schema.type/eligibility-criterion
    :entity.schema/sub-type {:db/id    (d/tempid :db.part/user)
                             :db/ident :entity.schema.sub-type/risk-bands}
    :entity.schema/fields   #{{:db/id                    (d/tempid :db.part/user)
                               :field/schema             :eligibility-criterion/risk-bands
                               :field/entity-schema-type (d/tempid :db.part/user -1)
                               :field/nullable?          false}}}

   {:db/id                  (d/tempid :db.part/user)
    :db/ident               :entity.schema/eligibility-criterion-secured?
    :entity.schema/type     :entity.schema.type/eligibility-criterion
    :entity.schema/sub-type {:db/id    (d/tempid :db.part/user)
                             :db/ident :entity.schema.sub-type/secured?}
    :entity.schema/fields   #{{:db/id           (d/tempid :db.part/user)
                               :field/schema    :eligibility-criterion/secured?
                               :field/nullable? false}}}])

@(d/transact conn include-eligiblity-schema)

;;TODO add something into the validator such that it checks to see whether the type is a keyword.
(def modified-full-fc-entity
  {:entity/instant                               (Date.)
   :funding-channel/funding-channel-uuid         (UUID/randomUUID)
   :funding-channel/name                         "Dorset Rise Ltd"
   :funding-channel/referral-only?               false
   :funding-channel/scale-allocation-percentage? true
   :funding-channel/allocation-percentage        (bigdec 0.7)
   :funding-channel/eligibility-criterions       #{{:entity.schema/sub-type           :entity.schema.sub-type/risk-bands

                                                    :eligibility-criterion/risk-bands #{:risk-band/Aplus
                                                                                        :risk-band/A
                                                                                        :risk-band/B
                                                                                        :risk-band/C
                                                                                        :risk-band/D}}
                                                   {:entity.schema/sub-type         :entity.schema.sub-type/secured?
                                                    :eligibility-criterion/secured? false}}

   :funding-channel/concentration-limits         #{{:concentration-limit/threshold             (float 0.7)
                                                    :concentration-limit/constraint-type       "MaxAllocationLifetime"
                                                    :concentration-limit/constrained-attribute ":original-principal-cents"
                                                    :concentration-limit/constrained-value     "1000"}

                                                   {:concentration-limit/threshold             (float 0.7)
                                                    :concentration-limit/constraint-type       "MaxAllocationMonthly"
                                                    :concentration-limit/constrained-attribute ":original-principal-cents"
                                                    :concentration-limit/constrained-value     "100"}}})

;; TODO we also want the possiblity of changing those types as a keyword
(->> (v/validate (d/db conn) :entity.schema.type/funding-channel modified-full-fc-entity)
     (v/valid?))