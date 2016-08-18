(ns entity-schema.example.house-prices.schema-data
  (:require [io.rkn.conformity :as c]
            [entity-schema.datomic.datomic-helper :as dh]
            [datomic.api :as d]))



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




(defn to-datomic [db {:keys [:db/ident :entity.schema/fields :entity.schema/natural-key :entity.schema/part] :as schema}]
  (assert (dh/all-indexed? db natural-key) (str "All natural key attributes should be indexed " natural-key "\n" schema))
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
    :db/index              true
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
                                 :field/nullable? true}

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

(defn enum-schemas [db]
  (->> ["age" "duration" "record-status" "category-type" "property-type"]
       (map (fn [x] (to-datomic db (enum-with-code-schema x :db.part/user))))))
