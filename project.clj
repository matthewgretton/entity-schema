(defproject entity-schema "0.1.0-SNAPSHOT"
  :description "Entity Schema demo"

  :repositories {"my.datomic.com" {:url "https://my.datomic.com/repo"
                                   :creds :gpg}}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.datomic/datomic-pro "0.9.5359"]
                 [cursive/datomic-stubs "0.9.5153" :scope "provided"]
                 [clj-yaml "0.4.0"]
                 [spyscope "0.1.5"]])
