(ns entity-schema.datomic.entity-schema-modification
  (:require [datomic.api :as d]))


(defn set-nullibility?-tx [db entity-id field-id nullable?]
  (->> (d/q '[:find ?f
              :in $ ?e ?s
              :where [?e :entity.schema/fields ?f]
              [?f :field/schema ?s]] db entity-id field-id)
       (first)
       (map (fn [f]
              [:db/add f :field/nullable? nullable?]))
       (first)))
