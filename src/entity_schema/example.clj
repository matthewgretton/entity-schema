(ns entity-schema.example
  (:require [entity-schema.from-yaml :as fy]
            [entity-schema.entity-schema :as es]
            [datomic.api :as d]
            [entity-schema.validator :as v])
  (:import (java.util Date)))

(defn boot-strap-conn []
  (let [uri "datomic:mem://entity-db"]
    (d/delete-database uri)
    (d/create-database uri)
    (es/bootstrap (d/connect uri))                          ;;connect caches connection
    (d/connect uri)))

(defn modify-entity-schema-optionality-tx [db entity-id field-id required?]
  (->> (d/pull db
               [{:entity.schema/fields [:db/id
                                        {:field/schema [:db/ident]}]}] entity-id)
       :entity.schema/fields
       (filter (fn [f]
                 (= field-id
                    (get-in f [:field/schema :db/ident]))))
       (map (fn [{:keys [:db/id]}]
              [:db/add id :field/required? required?]))
       (into [])
       ))

;; create connection and boot-strap with enity schema fields
(def conn (boot-strap-conn))

;; get clojure data structure from yaml file
(def yaml (fy/clojure-from-yaml "dimensions/concentration_limit_dim.yml"))

;; create datomic field transactions based on yaml
(def datomic-field-txs (fy/datomic-fields-from-yaml yaml))

;; transact fields
@(d/transact conn datomic-field-txs)

;; get transaction data for entity-schema based on the yaml
(def entity-schema-tx (fy/datomic-entity-schema-from-yaml yaml))

;; transact entity-schema transaction data
@(d/transact conn [entity-schema-tx])

;; get db/ident field for entity schema (this is a domain specific id for the entity)
(def db-ident (fy/entity-schema-ident-from-yaml yaml))



(def db-snapshot-0
  (d/db conn))

;; get unexpanded schema
(def unexpanded-schema
  (es/pull-unexpanded-schema db-snapshot-0 db-ident))

;; get expanded schema
(def expanded-schema
  (es/pull-expanded-schema db-snapshot-0 db-ident))


(def example-concentration-limit
  {:event/instant                                 (Date.)
   :db/ident                                      :entity.schema/concentration-limit-dim
   :concentration-limit-dim/threshold             (float 0.7)
   :concentration-limit-dim/constraint-type       "NotSureWhatGoesHere"
   :concentration-limit-dim/constrained-attribute "Blah"})

;; validate example
(v/non-back-compatible-validate db-snapshot-0 example-concentration-limit)







;;Make a non-back compatible change to the schema. Making a non required field required.
(def non-back-compat-tx
  (modify-entity-schema-optionality-tx db-snapshot-0
                                       :entity.schema/concentration-limit-dim
                                       :concentration-limit-dim/constrained-value
                                       true))


@(d/transact conn non-back-compat-tx)

(def db-snapshot-1 (d/db conn))

;;run to see change difference between db snapshots
(es/pull-unexpanded-schema db-snapshot-0 db-ident)
(es/pull-unexpanded-schema db-snapshot-1 db-ident)


(v/non-back-compatible-validate db-snapshot-1 ex)



















