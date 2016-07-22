(ns entity-schema.datomic.entity-schema
  (:require [datomic.api :as d]
            [entity-schema.datomic.datomic-helper :as dh]))


;TODO add in a check that there is at least one item in the entity
(defn build-query-map [db natural-key entity]
  (assert (some (fn [k] (contains? entity k)) natural-key) (str "There needs to be at least of the natural keys in the map" "\n"

                                                                "natural-key:" "\n" (with-out-str (clojure.pprint/pprint natural-key)) "\n"

                                                                "entity:" "\n" (with-out-str (clojure.pprint/pprint entity))))
  (let [ent-sym '?e
        [where in args] (->> (map vector natural-key (range (count natural-key)))
                             (reduce
                               (fn [[wh in args] [att i]]
                                 (if-let [v (get entity att)]
                                   (let [sym (symbol (str "?" i))]
                                     [(conj wh [ent-sym att sym]) (conj in sym) (conj args v)])
                                   [(conj wh (list 'not [ent-sym att '_])) in args]))
                               [[] ['$] [db]]))]
    {:query {:find  [ent-sym]
             :in    in
             :where where}
     :args  args}))


;; Methods to implement for a different database type
(defn ->entity-schema [db entity-schema-id]
  "Pull the whole schema for the Id"
  (-> (d/pull db '[:db/ident
                   {:entity.schema/type [:db/ident]}
                   {:entity.schema/part [:db/ident]}
                   {:entity.schema/sub-type [:db/ident]}
                   {:entity.schema/fields [{:field/schema [:db/ident
                                                           {:db/cardinality [:db/ident]}
                                                           {:db/valueType [:db/ident]}]}
                                           {:field/entity-schema-type [:db/ident]}
                                           :field/nullable?]}
                   {:entity.schema/natural-key [{:list/first [:db/ident]}
                                                {:list/rest ...}]}] entity-schema-id)
      (update :entity.schema/natural-key dh/from-linked-list-entity)))

(defn ->entity-schema-ids
  "Get the entity schema id/ids for supplied type/type & sub-type"
  ([db schema-type entity-type]
   (->> (d/q '[:find ?e
               :in $ ?s-type ?e-type
               :where
               [?e :entity.schema/type ?s-type]
               [?e :entity.schema/sub-type ?e-type]] db schema-type entity-type)
        (map first)
        (into #{})))
  ([db schema-type]
   (->> (d/q '[:find ?e
               :in $ ?type
               :where [?e :entity.schema/type ?type]] db schema-type)
        (map first)
        (into #{}))))

(defn ->sub-type-id [db entity]
  "Derive the sub-type entity id from the entity"
  (:entity.schema/sub-type entity))


(defn ->new-db-id [db part]
  "Create a new entity id"
  (d/tempid part))


(defn ->entity-id [db natural-key entity]
  "Look up the entity id based on the natural key and specified entity"
  (let [query-map (build-query-map db natural-key entity)
        r (d/query query-map)]
    (if (not (empty? r))
      (do (assert (= 1 (count r)) (str "Query result should only return one result \n"
                                       "natural-key:\n" (with-out-str (clojure.pprint/pprint natural-key)) "\n"
                                       "entity:\n" (with-out-str (clojure.pprint/pprint entity)) "\n"
                                       "query-map\n" (with-out-str (clojure.pprint/pprint query-map)) "\n"
                                       "result:\n" (with-out-str (clojure.pprint/pprint r))))

          (->> r (first) (first))))))


