(ns entity-schema.example.house-prices.fields
  (:require [datomic.api :as d]
            [entity-schema.datomic.datomic-helper :as dh]
            [entity-schema.datomic.entity-schema-data :as esd]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [entity-schema.datomic.entity-schema :as es]
            [entity-schema.processor :as p]
            [entity-schema.util :as u]
            [io.rkn.conformity :as c]
    ; [clojure.core.async :as a :refer (>!! <! >! go-loop)]
            [datomic.api :as d]
            [clojure.core.reducers :as r])
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


(require '[datomic.api :as d])
(def uri "datomic:mem://my-project")
(d/delete-database uri)
(d/create-database uri)
(def conn (d/connect uri))

; Hook up conformity and your sample datom
(def norms-map1 {:entity-schema/schema
                 {:txes [[{:db/id                 (d/tempid :db.part/db)
                           :db/ident              :something/title
                           :db/valueType          :db.type/string
                           :db/cardinality        :db.cardinality/one
                           :db/index              true
                           :db.install/_attribute :db.part/db}
                          {:db/id                 (d/tempid :db.part/db)
                           :db/ident              :something/title2
                           :db/valueType          :db.type/string
                           :db/cardinality        :db.cardinality/one
                           :db/index              true
                           :db.install/_attribute :db.part/db}]
                         ]}})

(def norms-map2 {:entity-schema/schema2
                 {:txes [[{:db/id                 :something/title2
                           :db/cardinality        :db.cardinality/one
                           :db.install/_attribute :db.part/db}]
                         ]}})








; -> false
(c/ensure-conforms conn norms-map1 [:entity-schema/schema])
(c/ensure-conforms conn norms-map2 [:entity-schema/schema2])


(c/has-attribute? (d/db conn) :something/title)

(d/pull (d/db conn) '[*] :something/title)




(defn to-datomic [db {:keys [:db/ident :entity.schema/fields :entity.schema/natural-key :entity.schema/part]}]
  (assert (dh/all-indexed? db natural-key) (str "All natural key attributes should be indexed " natural-key))
  {:db/id                     (d/tempid :db.part/entity-schema)
   :db/ident                  ident
   :entity.schema/part        part
   :entity.schema/fields      (->> fields
                                   (map #(assoc % :db/id (d/tempid part)))
                                   (into #{}))
   :entity.schema/natural-key (dh/build-datomic-linked-list part natural-key)
   })



(dh/build-datomic-linked-list :db.part/entity-schema
                              [:address/postcode :address/PAON :address/SAON :address/street])

(defn enum-with-code-schema [entity-type part]
  {:db/ident                  (keyword "entity.schema" entity-type)
   :entity.schema/part        part
   :entity.schema/fields      #{
                                {:field/schema    (keyword entity-type "code")
                                 :field/nullable? false}
                                }

   :entity.schema/natural-key [(keyword entity-type "code")]})


(declare structure-row)

(defn get-field-value [db field flat-data]
  (let [ident (get-in field [:field/schema :db/ident])
        type (get-in field [:field/schema :db/valueType :db/ident])]
    (if (= type :db.type/ref)
      (let [schema (u/derive-schema db field flat-data)]
        (structure-row db (:db/ident schema) flat-data))
      (get flat-data ident))))



(defn structure-row [db schema-id flat-data]
  (->> (es/pull-entity-schema db schema-id)
       (:entity.schema/fields)
       (map (fn [f]
              [(get-in f [:field/schema :db/ident]) (get-field-value db f flat-data)]))
       (filter (comp not nil? second))
       (into {})))

(defn structure-rows [db scheam-id data]
  (r/foldcat (r/map (fn [row] (structure-row db scheam-id row)) data)))







(def path "/Users/matthewgretton/Documents/Projects/entity-schema/src/entity_schema/example/house_prices/pp-2016.csv")

(defn toDate [ldt]
  (Date/from (.toInstant (.atZone ldt (ZoneOffset/UTC)))))



(def type-functions
  {
   :db.type/keyword keyword
   :db.type/string  identity
   :db.type/boolean #(Boolean/valueOf %)
   :db.type/long    #(Long/valueOf %)
   :db.type/bigint  #(BigInteger/valueOf (Long/valueOf %))
   :db.type/float   #(Float/valueOf %)
   :db.type/double  #(Double/valueOf ^String %)
   :db.type/bigdec  #(BigDecimal/valueOf (Double/valueOf %))
   :db.type/instant #(toDate (LocalDateTime/parse % (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")))
   :db.type/uuid    #(UUID/fromString %)
   :db.type/uri     #(URI/create %)})




(def header
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
   :record-status/code])


(defn get-funcs [db header]
  (->> header
       (map (fn [ident] (d/pull db [{:db/valueType [:db/ident]}] ident)))
       (map (fn [field] (get type-functions (get-in field [:db/valueType :db/ident]))))
       (into [])))

(defn transfrom-row [header-functions row]
  (->> (map vector header-functions row)
       (map (fn [[f v]]
              (f v)))
       (into [])))

(defn transform-csv [db header data]
  (let [funcs (get-funcs db header)]
    (->> data
         (map (fn [row] (transfrom-row funcs row))))))



(def fields
  [
   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :ppd/transaction-unique-identifier
    :db/doc                "A reference number which is generated
                            automatically recording each published
                            sale.
                            The number is unique and will change
                            each time a sale is recorded."
    :db/valueType          :db.type/string
    :db/unique             :db.unique/identity
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :ppd/price
    :db/doc                "Sale price stated on the transfer deed."
    :db/valueType          :db.type/long
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :ppd/date-of-transfer
    :db/doc                "Date when the sale was completed, as stated on the transfer deed."
    :db/valueType          :db.type/instant
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }



   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :ppd/property-type
    :db/doc                "Reference to a property type entity"
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :property-type/code
    :db/doc                " D = Detached, S = Semi-Detached, T = Terraced, F = Flats/Maisonettes, O = Other
                           Note that:
                           - we only record the above categories to describe property type, we do not separately identify bungalows.
                           - end-of-terrace properties are included in the Terraced category above.
                           - ‘Other’ is only valid where the transaction relates to a property type that is not covered by existing values."
    :db/valueType          :db.type/string
    :db/unique             :db.unique/identity
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }




   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :ppd/age
    :db/doc                "Reference to an age entity"
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :age/code
    :db/doc                "Indicates the age of the property and applies to all price paid transactions, residential and non-residential.
                           Y = a newly built property, N = an established residential building"
    :db/valueType          :db.type/string
    :db/unique             :db.unique/identity
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :ppd/duration
    :db/doc                "Referene to a duration entity"
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :duration/code
    :db/doc                "Relates to the tenure: F = Freehold, L= Leasehold etc.
                           Note that Land Registry does not record leases of 7 years or less in the Price Paid Dataset."
    :db/valueType          :db.type/string
    :db/cardinality        :db.cardinality/one
    :db/unique             :db.unique/identity
    :db.install/_attribute :db.part/db
    }


   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :ppd/address
    :db/doc                "The address of the property"
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :address/postcode
    :db/doc                "This is the postcode used at the time of the original transaction. Note that postcodes can
                            be reallocated and these changes are not reflected in the Price Paid Dataset."
    :db/valueType          :db.type/string
    :db/index              true
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :address/PAON
    :db/doc                "Primary Addressable Object Name.
                           If there is a sub-building for example the building is
                           divided into flats, see Secondary Addressable Object Name (SAON)."
    :db/valueType          :db.type/string
    :db/index              true
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :address/SAON
    :db/doc                "Secondary Addressable Object Name. If there is a sub-building, for example the building is
                            divided into flats, there will be a SAON."
    :db/valueType          :db.type/string
    :db/cardinality        :db.cardinality/one
    :db/index              true
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :address/street
    :db/valueType          :db.type/string
    :db/cardinality        :db.cardinality/one
    :db/index true
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :address/locality
    :db/valueType          :db.type/string
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :address/town_or_city
    :db/valueType          :db.type/string
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :address/district
    :db/valueType          :db.type/string
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :address/county
    :db/valueType          :db.type/string
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :ppd/category-type
    :db/doc                "A reference to a category type"
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :category-type/code
    :db/doc                "Indicates the type of Price Paid transaction.
                            A = Standard Price Paid entry, includes single residential property sold for full market value.
                            B = Additional Price Paid entry including transfers under a power of sale/repossessions,
                            buy-to-lets (where they can be identified by a Mortgage) and transfers to non-private individuals.

                           Note that category B does not separately identify the transaction types stated.
                           Land Registry has been collecting information on Category A transactions from January 1995. Category B transactions were identified from October 2013."
    :db/valueType          :db.type/string
    :db/unique             :db.unique/identity
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :ppd/record-status
    :db/doc                "A reference to a record status"
    :db/valueType          :db.type/ref
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }

   {
    :db/id                 (d/tempid :db.part/db)
    :db/ident              :record-status/code
    :db/doc                " Indicates additions, changes and deletions to the records.(see guide below).

                           A = Addition
                           C = Change
                           D = Delete.

                          Note that where a transaction changes category type due to misallocation (as above) it will be deleted from the
                          original category type and added to the correct category with a new transaction unique identifier."
    :db/valueType          :db.type/string
    :db/unique             :db.unique/identity
    :db/cardinality        :db.cardinality/one
    :db.install/_attribute :db.part/db
    }

   ])

(def enum-data
  [
   {:db/id              (d/tempid :db.part/user)
    :property-type/code "D"
    :db/ident           :property-type/detached}

   {:db/id              (d/tempid :db.part/user)
    :property-type/code "S"
    :db/ident           :property-type/semi-detached}

   {:db/id              (d/tempid :db.part/user)
    :property-type/code "T"
    :db/ident           :property-type/terraced}

   {:db/id              (d/tempid :db.part/user)
    :property-type/code "F"
    :db/ident           :property-type/flats_maisonettes}

   {:db/id              (d/tempid :db.part/user)
    :property-type/code "O"
    :db/ident           :property-type/other}

   {:db/id    (d/tempid :db.part/user)
    :age/code "Y"
    :db/ident :age/newly-built-property}

   {:db/id    (d/tempid :db.part/user)
    :age/code "N"
    :db/ident :age/established-residential-build}

   {:db/id         (d/tempid :db.part/user)
    :duration/code "F"
    :db/ident      :duration/freehold}

   {:db/id         (d/tempid :db.part/user)
    :duration/code "L"
    :db/ident      :duration/leasehold}

   {:db/id              (d/tempid :db.part/user)
    :db/ident           :category-type/standard-price-paid
    :category-type/code "A"}

   {:db/id              (d/tempid :db.part/user)
    :db/ident           :category-type/additional-price-paid
    :category-type/code "B"}

   {:db/id              (d/tempid :db.part/user)
    :db/ident           :record-status/addition
    :record-status/code "A"}

   {:db/id              (d/tempid :db.part/user)
    :db/ident           :record-status/change
    :record-status/code "C"}

   {:db/id              (d/tempid :db.part/user)
    :db/ident           :record-status/delete
    :record-status/code "D"}

   ])



(def address-schema
  {:db/ident                  :entity.schema/address
   :entity.schema/part        :db.part/user
   :entity.schema/fields      #{
                                {:field/schema    :address/county
                                 :field/nullable? true}

                                {:field/schema    :address/district
                                 :field/nullable? true}

                                {:field/schema    :address/locality
                                 :field/nullable? true}

                                {:field/schema    :address/postcode
                                 :field/nullable? false}

                                {:field/schema    :address/PAON
                                 :field/nullable? false}

                                {:field/schema    :address/SAON
                                 :field/nullable? false}

                                {:field/schema    :address/street
                                 :field/nullable? true}

                                {:field/schema    :address/town_or_city
                                 :field/nullable? true}}

   :entity.schema/natural-key [:address/postcode :address/PAON :address/SAON :address/street]
   })







(def ppd-schema
  {:db/ident                  :entity.schema/ppd
   :entity.schema/part        :db.part/user
   :entity.schema/fields      #{
                                {:field/schema    :ppd/transaction-unique-identifier
                                 :field/nullable? false}

                                {:field/schema        :ppd/address
                                 :field/entity-schema :entity.schema/address
                                 :field/nullable?     false}


                                {:field/schema        :ppd/age
                                 :field/entity-schema :entity.schema/age
                                 :field/nullable?     false}

                                {:field/schema        :ppd/category-type
                                 :field/entity-schema :entity.schema/category-type
                                 :field/nullable?     false}

                                {:field/schema    :ppd/date-of-transfer
                                 :field/nullable? false}

                                {:field/schema        :ppd/duration
                                 :field/entity-schema :entity.schema/duration
                                 :field/nullable?     false}

                                {:field/schema        :ppd/property-type
                                 :field/entity-schema :entity.schema/property-type
                                 :field/nullable?     false}

                                {:field/schema        :ppd/record-status
                                 :field/entity-schema :entity.schema/record-status
                                 :field/nullable?     false}

                                {:field/schema    :ppd/price
                                 :field/nullable? false}
                                }

   :entity.schema/natural-key [:ppd/transaction-unique-identifier]
   })

(def uri "datomic:mem://price-paid-data")

(d/delete-database uri)

(d/create-database uri)

(def conn (d/connect uri))

@(d/transact conn fields)

@(d/transact conn esd/all-fields)





(def enum-schema
  {:db/id                     (d/tempid :db.part/entity-schema)
   :db/ident                  :entity.schema/enum
   :entity.schema/part        :db.part/entity-schema
   :entity.schema/fields      #{{:field/schema    :db/ident
                                 :field/nullable? false}}
   :entity.schema/natural-key [:db/ident]})

(def datomic-field-schema
  {:db/id                (d/tempid :db.part/entity-schema)
   :db/ident             :entity.schema/datomic-field
   :entity.schema/part   :db.part/entity-schema
   :entity.schema/fields #{
                           {:db/id                (d/tempid :db.part/entity-schema)
                            :field/schema    :db/ident
                            :field/nullable? false}

                           {:db/id                (d/tempid :db.part/entity-schema)
                            :field/schema        :db/valueType
                            :field/entity-schema enum-schema
                            :field/nullable?     false}

                           {:db/id                (d/tempid :db.part/entity-schema)
                            :field/schema        :db/cardinality
                            :field/entity-schema enum-schema
                            :field/nullable?     false}

                           {:db/id                (d/tempid :db.part/entity-schema)
                            :field/schema        :db/index
                            :field/nullable?     true}

                           {:db/id                (d/tempid :db.part/entity-schema)
                            :field/schema        :db/unique
                            :field/entity-schema enum-schema
                            :field/nullable?     true}
                           }
   })



(def field-schema
  {:db/ident                  :entity.schema/field
   :entity.schema/fields      #{{:field/schema        :field/schema
                                 :field/entity-schema {}
                                 :field/nullable?     false}
                                {:field/schema    :field/nullable?
                                 :field/nullable? false}}
   :entity.schema/natural-key []})


(def entity-schema-schema
  )






















@(d/transact conn enum-data)

(def enum-schemas
  (->> ["age" "duration" "record-status" "category-type" "property-type"]
       (map (fn [x] (to-datomic (d/db conn) (enum-with-code-schema x :db.part/user))))))

@(d/transact conn enum-schemas)

@(d/transact conn [(to-datomic (d/db conn) address-schema)])


@(d/transact conn [(to-datomic (d/db conn) ppd-schema)])




(defn process-data [db header data]
  (->> data
       (transform-csv db header)
       (map #(zipmap header %))))


(defn process-csv [db header csv-path]
  (with-open [in-file (io/reader csv-path)]
    (->> (csv/read-csv in-file)
         (take 4000)
         (process-data db header)
         (into [])
         (structure-rows db :entity.schema/ppd)
         (into [])
         ;;TODO going to have to make the command stuff compatible with schema types.
         (p/process-all-entities (d/db conn)
                                 :entity.schema/ppd
                                 {:command-map     {:entity.schema/ppd     :command/insert
                                                    :entity.schema/address :command/insert}
                                  :default-command :command/look-up}))))




;
;
; 16000 -> 48722
; 32000 ->
; 389034 - 1839.396

;


(def process-result (time (process-csv (d/db conn) header path)))
;
;(first (p/get-entities-from-process-result process-result))
;

;(def txs (p/get-entities-from-process-result process-result))


;@(d/transact conn txs)
;
;
;(def txs-8000 (time (process-csv (d/db conn) header path)))
;
;




