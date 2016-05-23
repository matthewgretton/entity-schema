(ns entity-schema.example.evolving-schema-and-code-example
  (:require [entity-schema.yaml-conversion :as fy]
            [datomic.api :as d]
            [entity-schema.validation :as v]
            [entity-schema.entity-schema :as es]
            [entity-schema.datomic-helper :as dh])
  (:import (java.util Date UUID)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This example demonstrates the following
;
; - Validation of an entity
; - Evolving the schema over time
; - Evolving code and schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;;Let's look up the schema we have created. First we use the :db/ident attribte of fc-schema-tx to look it up a
;; :db/ident way of unique identifying entities in datomic
(es/pull-schema-by-id (d/db conn) :entity.schema/funding-channel)

;;We can also look up by entity schema type, we currently have only one schema of this type so only one is returned
(es/pull-schema-by-type (d/db conn) :entity.schema.type/funding-channel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Validation, Evolving entity schema and code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def instant-0 (Date.))

;; create a test funding circle entity
(def fc-entity
  {
   :funding-channel/funding-channel-uuid         (UUID/randomUUID)
   :funding-channel/referral-only?               false
   :funding-channel/scale-allocation-percentage? true
   :funding-channel/allocation-percentage        0.7M
   }
  )

;; entity invalid as it is missing :funding-channel/name
(v/assert-valid (d/db conn) :entity.schema.type/funding-channel fc-entity)

;; Make :funding-channel/name nullable
@(d/transact conn [(es/set-nullibility?-tx (d/db conn)
                                           :entity.schema/funding-channel
                                           :funding-channel/name
                                           true)])

;;See that :funding-channel/name is now nullable
(es/pull-schema-by-id (d/db conn) :entity.schema/funding-channel)

;;should now be valid
(v/assert-valid (d/db conn) :entity.schema.type/funding-channel fc-entity)

;; However if we use a database asof a time before we made the change, it's still invalid as expected
(-> (d/as-of (d/db conn) instant-0)
    (v/assert-valid :entity.schema.type/funding-channel fc-entity))



;; Using this technique we can validate consistently with respect to time, however that's not the entire story.
;; In reality, schema changes are only needed when code that uses those entities changes. So ideally, you would want
;; to evolve code with schema changes atomically. Datomic has a function type so we can in fact do this. For example,
;; business logic on the Janus platform is contained in stateless handler functions (both tublines and samza)


;; As an example let's define a handler function that does something with an entity.
;; We use defn-db which is just a convenience macro for creating a function that can be stored in the database.
;; Equivalently we can use the datomic.api/function method directly by passing a map containing info on the function

;(def hndlr-v1
;  (d/function {:lang     :clojure                           ; can also define java functions
;               :params   ['entity]
;               :code     "(do (if (contains? entity :funding-channel/name) (:funding-channel/name entity) \"Default\"))"}
;              ))

(dh/defn-db hndlr-v1 [entity]
  (if (contains? entity :funding-channel/name)
    (:funding-channel/name entity)
    "Default"))

;;it's a normal function
(hndlr-v1 {:funding-channel/name "bob"})                    ;bob
(hndlr-v1 {})                                               ;Default

;; :db/fn is a standard datomic field used for storing functions, the field is of type :db.type/fn. We identify the
;; function entity using the :db/ident attribute as mentioned above, this is like the function name. In this
;; case we have also added a comment using the db/doc field.
@(d/transact conn [{:db/id    (d/tempid :db.part/user)
                    :db/ident :handler-function/hndlr
                    :db/doc   "This handler function gets the name from the
                             funding channel entity or returns Default if it does not exists"
                    :db/fn    hndlr-v1}])

;; We can pull function from the database as any other type of data
(def hndlr
  (->> (d/pull (d/db conn) '[:db/fn] :handler-function/hndlr)
       :db/fn))


;; function from the database behaves as it did when we put it in.
(hndlr {:funding-channel/name "bob"})                       ;"bob
(hndlr {})                                                  ;"Default"

;; Let's define a simple function which takes a entity type id, a handler function id and an entity.
;; The function first validates the entity and then executes the function. The function that all entities passed to
;; it have an :entity/instant attribute that signifies when the entity
(defn validate-and-execute [db
                            schema-type
                            handler-function-id
                            {:keys [:entity/instant] :as entity}]
  (let [db-at-instant (d/as-of db instant)                  ;; get history consistent db
        hndlr-fn (->> (d/pull db-at-instant '[:db/fn] handler-function-id)
                      :db/fn)                               ;; get the history consistent function
        ]
    (v/assert-valid db-at-instant schema-type entity)       ;; note we use the standard assert as we are passing it a historical database
    (hndlr-fn entity)                                       ;execute function
    ))

(def instant-name-optional (Date.))



;; The database currently has f-c/name as optional so below should run without issue
(validate-and-execute (d/db conn)
                      :entity.schema.type/funding-channel
                      :handler-function/hndlr
                      (assoc fc-entity :entity/instant instant-name-optional))                         ;Default



;;TODO add a section relating to testing with (d/with ...)




;; Let's make a change to our handler function such that it will require a non backwards compatible change.
(dh/defn-db hndlr-v2 [entity]
            (assert (contains? entity :funding-channel/name)) ;; our schema now needs to enforce that name there.
            (:funding-channel/name entity))

(hndlr-v2 {:funding-channel/name "bob"})                    ;bob
(hndlr-v2 {})                                               ;blows up

;; Atomically change the function entity, and the entity schema. Note that if your schema change affected many handler
;; functions then you would need to change these other functions at the same time.
@(d/transact conn [
                   (es/set-nullibility?-tx (d/db conn)
                                           :entity.schema/funding-channel
                                           :funding-channel/name
                                           false)           ; schema change
                   {:db/id :handler-function/hndlr :db/fn hndlr-v2} ;reset handler functions
                   ;{:db/id :handler-function/hndlr-x :db/fn handle-x}
                   ;...
                              ;update schema
                   ])

(def instant-name-required (Date.))


;; Should fail validation as the name is missing
(validate-and-execute (d/db conn) :entity.schema.type/funding-channel :handler-function/hndlr
                      (assoc fc-entity :entity/instant instant-name-required))

;;Should execute fine as the :entity/instant was before we made name required.
(validate-and-execute (d/db conn) :entity.schema.type/funding-channel :handler-function/hndlr
                      (assoc fc-entity :entity/instant instant-name-optional)) ;Default















