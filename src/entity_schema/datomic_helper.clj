(ns entity-schema.datomic-helper
  (:require [datomic.api :as d]))

(defn create-in-mem-db-uri [db-name]
  (str "datomic:mem://" db-name "-" (d/squuid)))


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

(defn look-up-entity-by-natural-key [db natural-key entity]
  (let [query-map (build-query-map db natural-key entity)
        r (d/query query-map)]
    (if (not (empty? r))
      (do (assert (= 1 (count r)) (str "Query result should only return one result \n"
                                       "natural-key:\n" (with-out-str (clojure.pprint/pprint natural-key)) "\n"
                                       "entity:\n" (with-out-str (clojure.pprint/pprint entity)) "\n"
                                       "query-map\n" (with-out-str (clojure.pprint/pprint query-map)) "\n"
                                       "result:\n" (with-out-str (clojure.pprint/pprint r))
                                       ))
          (->> r (first) (first))))))




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


(def bob (build-datomic-linked-list :db.part/user [:bob :ted]))

(from-linked-list-entity bob)







