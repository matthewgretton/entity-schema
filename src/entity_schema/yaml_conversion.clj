(ns entity-schema.yaml-conversion
  (:require [clj-yaml.core :as yaml]
            [datomic.api :as d]
            [entity-schema.entity-schema :as es]))


(def yaml-datomic-type-map
  {"bigserial" :db.type/long
   "real"      :db.type/float
   "uuid"      :db.type/uuid
   "string"    :db.type/string
   "numeric"   :db.type/bigdec
   "boolean"   :db.type/boolean
   "timestamp" :db.type/instant})

(defn prettify-name [name]
  (-> name
      (clojure.string/replace "_" "-")
      (clojure.string/replace " " "-")
      (clojure.string/replace "-dim" "")
      (clojure.string/lower-case)))

(defn to-coll
  [item]
  (if (coll? item)
    item
    #{item}))

(defn create-db-ident [entity-name field-name]
  (let [ns (prettify-name entity-name)
        name (prettify-name field-name)]
    (keyword ns
             (clojure.string/replace name (str ns "-") ""))))

(defn create-datomic-field [entity-name
                            indexes
                            natural-key
                            {:keys [:name
                                    :data_type
                                    :description]}]
  (let [natural-key-set (into #{} (to-coll natural-key))
        index-set (into natural-key-set (to-coll indexes))
        datomic-field {:db/id                 (d/tempid :db.part/db)
                       :db/ident              (create-db-ident entity-name name)
                       :db/cardinality        :db.cardinality/one
                       :db/valueType          (if (contains? yaml-datomic-type-map data_type)
                                                (get yaml-datomic-type-map data_type)
                                                (assert false (str data_type " expectected to be mapped in "
                                                                   yaml-datomic-type-map)))
                       :db/doc                description
                       :db.install/_attribute :db.part/db}]
    (if (contains? index-set name)
      (if (= 1 (count natural-key-set))
        (assoc datomic-field :db/unique :db.unique/identity)
        (assoc datomic-field :db/index true))
      datomic-field)))

(defn yaml->field-txs [{:keys [:name :columns :natural_key :indexes] :as yaml}]
  (->> columns
       (filter #(not= (:name %) "id"))                      ;id field will be created automatically
       (map #(create-datomic-field name indexes natural_key %))
       (into [])))

(defn read-yaml-from-class-path [path]
  (->> (clojure.java.io/resource path)
       (slurp)
       (yaml/parse-string)))

(defn create-entity-schema-field [entity-name {:keys [:name :nullable]}]
  {:db/id           (d/tempid :db.part/user)
   :field/schema    (create-db-ident entity-name name)
   :field/nullable? nullable})



(defn yaml->entity-schema-tx [{:keys [:name :natural_key :columns]}]
  {
   :db/id                     (d/tempid :db.part/user)

   :db/ident                  (keyword "entity.schema" (prettify-name name))

   :entity.schema/type        {:db/id    (d/tempid :db.part/user)
                               :db/ident (keyword "entity.schema.type" (prettify-name name))}

   :entity.schema/fields      (->> columns
                                   (filter #(not= (:name %) "id"))
                                   (map #(create-entity-schema-field name %))
                                   (into #{}))

   :entity.schema/natural-key (->> (to-coll natural_key)
                                   (map #(create-db-ident name %))
                                   (into #{}))
   })







