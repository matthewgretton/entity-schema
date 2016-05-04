(ns entity-schema.printing-tools
  (:import (java.io StringWriter)))

(defn pprint [object]
  (let [writer (new StringWriter)]
    (clojure.pprint/pprint object writer)
    (.. writer getBuffer toString)))