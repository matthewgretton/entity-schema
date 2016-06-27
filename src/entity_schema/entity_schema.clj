(ns entity-schema.entity-schema)


;; Define a protocol - we could use this to decouple the entity schema code from the datomic
;; implementation.

(defprotocol EntitySchemaDatabase

  "Implementation details of database access for entity"

  (->entity-schema [db id] "Pull the whole schema for the Id")

  (->new-db-id [db partition])

  (->sub-type-id [db entity] "Derive the sub-type entity id from the entity")

  (->entity-schema-ids
    [db entity-schema-type-id] [db entity-schema-type-id entity-schema-sub-type-id]
    "Get the entity schema id/ids for supplied type/type & sub-type")

  (->entity-id [db natural-key-list entity] "Look up entity id by natural key")

  )
