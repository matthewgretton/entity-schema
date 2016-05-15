(ns entity-schema.datomic-helper
  (:require [datomic.api :as d]))

(defn create-in-mem-db-uri [db-name]
  (str "datomic:mem://" db-name "-" (d/squuid)))

