(ns entity-schema.example.house-prices.fields
  (:require [datomic.api :as d]
            [entity-schema.datomic.entity-schema-data :as esd]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [entity-schema.processor :as p]
            [entity-schema.util :as u]
            [datomic.api :as d]
            [clojure.core.reducers :as r]
            [entity-schema.validation :as v]
            [entity-schema.example.house-prices.schema-data :as sd])
  (:import (java.util UUID Date)
           (java.net URI)
           (java.time.format DateTimeFormatter)
           (java.time LocalDateTime ZoneOffset)))



;(defn tx-pipeline
;  "Transacts data from from-ch. Returns a map with:
;     :result, a return channel getting {:error t} or {:completed n}
;     :stop, a fn you can use to terminate early."
;  [conn conc from-ch]
;  (let [to-ch (a/chan 100)
;        done-ch (a/chan)
;        transact-data (fn [data]
;                        (try
;                          @(d/transact-async conn data)
;                          ; if exception in a transaction
;                          ; will close channels and put error
;                          ; on done channel.
;                          (catch Throwable t
;                            (.printStackTrace t)
;                            (a/close! from-ch)
;                            (a/close! to-ch)
;                            (>!! done-ch {:error t}))))]
;
;    ; go block prints a '.' after every 1000 transactions, puts completed
;    ; report on done channel when no value left to be taken.
;    (go-loop [total 0]
;             (when (zero? (mod total 1000))
;               (print ".") (flush))
;             (if-let [c (<! to-ch)]
;               (recur (inc total))
;               (>! done-ch {:completed total})))
;
;    ; pipeline that uses transducer form of map to transact data taken from
;    ; from-ch and puts results on to-ch
;    (a/pipeline-blocking conc to-ch (map transact-data) from-ch)
;
;    ; returns done channel and a function that you can use
;    ; for early termination.
;    {:result done-ch
;     :stop (fn [] (a/close! to-ch))}))



(def uri "datomic:mem://price-paid-data")

(d/delete-database uri)

(d/create-database uri)

(def conn (d/connect uri))

@(d/transact conn sd/fields)
@(d/transact conn esd/all-fields)
@(d/transact conn sd/enum-data)
@(d/transact conn (sd/enum-schemas (d/db conn)))
@(d/transact conn [(sd/to-datomic (d/db conn) sd/address-schema)])
@(d/transact conn [(sd/to-datomic (d/db conn) sd/ppd-schema)])






(def path "/Users/matthewgretton/Documents/Projects/entity-schema/src/entity_schema/example/house_prices/pp-2016.csv")

(defn toDate [ldt]
  (Date/from (.toInstant (.atZone ldt (ZoneOffset/UTC)))))


(defn build-key-func-pairs [db header]
  (let [type-functions {:db.type/keyword keyword
                        :db.type/string  identity
                        :db.type/boolean #(Boolean/valueOf %)
                        :db.type/long    #(Long/valueOf %)
                        :db.type/bigint  #(BigInteger/valueOf (Long/valueOf %))
                        :db.type/float   #(Float/valueOf %)
                        :db.type/double  #(Double/valueOf ^String %)
                        :db.type/bigdec  #(BigDecimal/valueOf (Double/valueOf %))
                        :db.type/instant #(toDate (LocalDateTime/parse % (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")))
                        :db.type/uuid    #(UUID/fromString %)
                        :db.type/uri     #(URI/create %)}]
    (->> header
         (map (fn [ident]
                (let [type (get-in (d/pull db [{:db/valueType [:db/ident]}] ident) [:db/valueType :db/ident])
                      func (get type-functions type)]
                  [ident func])))
         (into []))))

(defn nil?-or-empty? [v]
  (or (nil? v) (empty? v)))

(defn transform-csv-row [key-function-pairs row]
  (->> (map vector key-function-pairs row)
       (map (fn [[[k f] v]]
              [k (if (nil?-or-empty? v) nil (f v))]))
       (into {})))

(defn read-csv-into-vector [path]
  (with-open [in-file (io/reader path)]
    (->> (csv/read-csv in-file)
         (into []))))



(declare structure-row)

(defn get-field-value [db field flat-data]
  (let [ident (get-in field [:field/schema :db/ident])
        type (get-in field [:field/schema :db/valueType :db/ident])]
    (if (= type :db.type/ref)
      (structure-row db (:field/entity-schema field) flat-data)
      (get flat-data ident))))

(defn structure-row [db schema flat-data]
  (->> (:entity.schema/fields schema)
       (map (fn [f]
              [(get-in f [:field/schema :db/ident]) (get-field-value db f flat-data)]))
       (filter (comp not nil? second))
       (into {})))

(defn validate-schema [schema]
  ;;check that the schema is valid do this once.
  ;;(assert (dh/all-indexed? db natural-key) (str "All natural key attributes should be indexed " natural-key))
  true)

(defn process-grid-data [db schema-id command-data
                         header grid-data]
  (let [pairs (build-key-func-pairs db header)
        schema (u/recursively-pull-schema (d/db conn) schema-id {})]
    (validate-schema schema)
    (p/process-all-entities db schema command-data
                            (->> grid-data
                                 (r/map (partial transform-csv-row pairs))
                                 (r/map (partial structure-row db schema))))))

(def grid-data (time (into [] (take 10 (read-csv-into-vector path)))))

;
;
(def process-result (time (process-grid-data (d/db conn)

                                             :entity.schema/ppd

                                             {:command-map     {:entity.schema/ppd     :command/insert
                                                                :entity.schema/address :command/insert}
                                              :default-command :command/look-up}

                                             [:ppd/transaction-unique-identifier
                                              :ppd/price
                                              :ppd/date-of-transfer
                                              :address/postcode
                                              :property-type/code
                                              :age/code
                                              :duration/code
                                              :address/PAON
                                              :address/SAON
                                              :address/street
                                              :address/locality
                                              :address/town_or_city
                                              :address/district
                                              :address/county
                                              :category-type/code
                                              :record-status/code] grid-data)))







;
;(count (p/get-errors-from-process-result process-result))
;
;(p/get-entities-from-process-result process-result)
;
;(defn get-vals [m ks]
;  (->> ks
;       (map #(get m %))
;       (filter (comp not nil?))
;       (filter (comp not v/error?))
;       (into [])))
;
;(defn construct-str-address [address-entity]
;  (let [keys [:address/PAON :address/SAON :address/street :address/postcode]]
;    (clojure.string/join ", " (get-vals address-entity keys))))
;
;
;
;
;(->> (p/get-errors-from-process-result process-result)
;     (map #(construct-str-address (:ppd/address %)))
;     (into []))
;
;;;TODO it would be nice if we could identify what line of the input the errors occor, and also what the type of
;;;TODO error is
;;
;;
;;
;;
;(def txs (p/get-entities-from-process-result process-result))
;;
;;
;;
;;
;;
;@(d/transact conn txs)
;;
;;
;;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;;;Test stuff that we should really put in a test on there own.
;
; (assert (= [[{:field/schema {:db/ident :ppd/transaction-unique-identifier,
;                              :db/cardinality {:db/ident :db.cardinality/one},
;                              :db/valueType {:db/ident :db.type/string}},
;               :field/nullable? false}]
;             [{:field/schema {:db/ident :ppd/date-of-transfer,
;                              :db/cardinality {:db/ident :db.cardinality/one},
;                              :db/valueType {:db/ident :db.type/instant}},
;               :field/nullable? false}
;              {:field/schema {:db/ident :ppd/price,
;                              :db/cardinality {:db/ident :db.cardinality/one},
;                              :db/valueType {:db/ident :db.type/long}},
;               :field/nullable? false}]]
;            (p/split-fields-by-natural-key #{
;
;                                 {:field/schema
;                                                   {:db/ident :ppd/date-of-transfer,
;                                                    :db/cardinality {:db/ident :db.cardinality/one},
;                                                    :db/valueType {:db/ident :db.type/instant}},
;                                  :field/nullable? false}
;                                 {:field/schema
;                                                   {:db/ident :ppd/transaction-unique-identifier,
;                                                    :db/cardinality {:db/ident :db.cardinality/one},
;                                                    :db/valueType {:db/ident :db.type/string}},
;                                  :field/nullable? false}
;
;
;                                 {:field/schema
;                                                   {:db/ident :ppd/price,
;                                                    :db/cardinality {:db/ident :db.cardinality/one},
;                                                    :db/valueType {:db/ident :db.type/long}},
;                                  :field/nullable? false}
;                                 }
;                               #{:ppd/transaction-unique-identifier})))
;
;
;
;
