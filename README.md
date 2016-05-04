# entity-schema

Small example app to demonstrate storing entity schema data in a database, and using that information for validation purposes. Also demonstrates how versioning can be easliy implemented to ensure validation is consistent historically.

# Installation

To pull down Datomic dependencies you need to read from a private repository. This requires authentication credentials. 

You will need [gpg installed](https://github.com/technomancy/leiningen/blob/stable/doc/GPG.md#installing-gpg) and a [key pair configured](https://github.com/technomancy/leiningen/blob/stable/doc/GPG.md#creating-a-keypair). 

Add the below to your credentials.clj map as described in the following [gpg guide](https://github.com/technomancy/leiningen/blob/master/doc/DEPLOY.md#gpg).


`{#"my\.datomic\.com" {:username "matthew.gretton@gmail.com" :password "e76febfa-e7aa-4520-8fe8-b8508fd62f09"}}`


Once you've done that, you should be able run

`lein deps`

to ensure you download dependencies.

