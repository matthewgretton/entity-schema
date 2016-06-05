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
       (datomic.api/function {:lang "clojure"
                              :params '~params
                              :requires ~(:requires attr-map)
                              :imports ~(:imports attr-map)
                              :code '(do ~@body)}))))


(defn build-query-map [db natural-key entity]
  (let [ent-sym '?e
        [where in args] (->> (map vector natural-key (range (count natural-key)))
                               (reduce
                                 (fn [[wh in prms] [att i]]
                                   (if-let [v (get entity att)]
                                     (let [sym (symbol (str "?" i))]
                                       [(conj wh [ent-sym att sym]) (conj in sym) (conj prms v)])
                                     [(list 'not [ent-sym att '_]) in prms]))
                                 [[] ['$] [db]]))]
    {:query {:find  [ent-sym]
             :in    in
             :where where}
     :args args}))




