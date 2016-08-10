(ns entity-schema.datomic.datomic-helper
  (:require [datomic.api :as d]))

(defn create-in-mem-db-uri [db-name]
  (str "datomic:mem://" db-name "-" (d/squuid)))


(defn to-linked-list [input]
  (loop [values (reverse input) result {}]
    (if (empty? values)
      result
      (let [stub {:list/first (first values)}
            new-result (if (empty? result) stub (assoc stub :list/rest result))]
        (recur (rest values) new-result)))))

(defn from-linked-list [ll]
  (loop [result [] current ll]
    (if (not (contains? current :list/first))
      result
      (recur (conj result (:list/first current)) (:list/rest current)))))

(defn index? [db att-id]
   (let [{:keys [:db/index :db/unique]} (d/pull db [:db/index :db/unique] att-id)]
     (or index unique)))

(defn all-indexed? [db atts]
  (every? (partial index? db) atts))

(defn build-datomic-linked-list [part actual-list]
  (assoc (to-linked-list actual-list) :db/id (d/tempid part)))


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









