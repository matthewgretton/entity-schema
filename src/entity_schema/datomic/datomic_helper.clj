(ns entity-schema.datomic.datomic-helper
  (:require [datomic.api :as d]))

(defn create-in-mem-db-uri [db-name]
  (str "datomic:mem://" db-name "-" (d/squuid)))


(defn from-linked-list-entity
  ([linked-list-entity]
   (from-linked-list-entity linked-list-entity :list/first))
  ([linked-list-entity first-key]
   (into [] (let [f (first-key linked-list-entity)]
              (if (contains? linked-list-entity :list/rest)
                (cons f (from-linked-list-entity (:list/rest linked-list-entity) first-key))
                [f])))))


(defn to-linked-list-entity
  ([v]
   (to-linked-list-entity v :list/first))
  ([v first-key]
   (let [f {first-key (first v)}
         r (rest v)]
     (if (empty? r) f (assoc f :list/rest (to-linked-list-entity r first-key))))))





(defn build-datomic-linked-list [part actual-list]
  (merge {:db/id (d/tempid part)}
         (to-linked-list-entity actual-list)))




(defn maybe-assoc [m k v]
  (if v (assoc m k v) m))

(defmacro defn-db [name & args]
  (let [[doc-string & args] (if (string? (first args)) args (cons nil args))
        [attr-map & args] (if (map? (first args)) args (cons {} args))
        [[& params] & body] args
        metadata (-> attr-map
                     (assoc :dbfn true)
                     (maybe-assoc :doc doc-string)
                     (dissoc :requires :imports))]
    `(def ~(vary-meta name merge metadata)
       (datomic.api/function {:lang     "clojure"
                              :params   '~params
                              :requires ~(:requires attr-map)
                              :imports  ~(:imports attr-map)
                              :code     '(do ~@body)}))))









