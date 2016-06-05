(ns entity-schema.processor
  (:require [entity-schema.entity-schema :as es]
            [datomic.api :as d]))



(declare process-entity)

(defn get-all [map keys]
  (->> keys
       (map (partial get map))
       (filter (comp not nil?))
       (into [])))

(defn diff [set1 set2]
  [(clojure.set/intersection set1 set2)
   (clojure.set/difference set1 set2)])


(for [x [1 2 3 4 5]
      y [1 2 3 4 5]]
  [x y])





