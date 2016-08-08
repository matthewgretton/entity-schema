# entity-schema

Small example app to demonstrate storing entity schema data in a database, and using that information for validation purposes. Also demonstrates how versioning can be easliy implemented to ensure validation is consistent historically.

# Usage

Give an example of a schema and also an example of an entity with a subtype.

```clojure
{:db/ident :entity.schema/test
 :entity.schema/fields #{{:field/schema {:db/ident :test/field1
                                         :db/ValueType {:db/ident :db.type/string}
                                          :db/cardinality {:db/ident  :db.cardinality/one}}
                          :field/nullable? false}}
 :entity.schema/natural-key [:test/field1]}
```

# Installation

To pull down Datomic dependencies you need to read from a private repository. This requires authentication credentials. 

You will need [gpg installed](https://github.com/technomancy/leiningen/blob/stable/doc/GPG.md#installing-gpg) and a [key pair configured](https://github.com/technomancy/leiningen/blob/stable/doc/GPG.md#creating-a-keypair). 

Add the below to your credentials.clj map as described in the following [gpg guide](https://github.com/technomancy/leiningen/blob/master/doc/DEPLOY.md#gpg).


`{#"my\.datomic\.com" {:username "matthew.gretton@gmail.com" :password "e76febfa-e7aa-4520-8fe8-b8508fd62f09"}}`


Once you've done that, you should be able run

`lein deps`

to ensure you download dependencies.


When using Datomic the symbols aren’t resolved in the editor or the REPL due to the fact that Datomic is distributed
without source code. To work around the issue do the following:

- Download https://cursive-ide.com/datomic-stubs-0.9.5153.jar
- Install that jar to your local repository cache using mvn install:install-file -Dfile=datomic-stubs-0.9.5153.jar -DartifactId=datomic-stubs -Dversion=0.9.5153 -DgroupId=cursive -Dpackaging=jar
- Add these stubs to your project: [cursive/datomic-stubs "0.9.5153" :scope "provided"]

