(ns entity-schema.example
  (:require [entity-schema.example-data :as ed]
            [datomic.api :as d]
            [entity-schema.entity-schema :as es]
            [clojure.walk :as w]))



(def uri "datomic:mem://entity-db")

(d/delete-database uri)

(d/create-database uri)

(def conn (d/connect uri))

(ed/bootstrap-example-conn conn)




(es/pull-unexpanded-schema (d/db conn) :entity.schema/test-entity1)

(es/pull-expanded-schema (d/db conn) :entity.schema/test-entity1)


(+ 7 6 8 )

(def bec-list [1 2 3 4 5])

(map inc bec-list)








