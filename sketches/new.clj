(ns cljplot-new
  (:require [clojure.data.csv :as csv]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [fastmath.core :as m]))


(defn read-json [f] (with-open [reader (io/reader f)]
                      (doall (json/read-json reader true))))


(defn read-csv [f] (rest (with-open [reader (io/reader f)]
                           (doall (csv/read-csv reader)))))

(defn map-kv [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))


(def chem97 (read-json "data/chem97.json"))

(sort (distinct (map :score chem97)))

(keys (into (sorted-map) (group-by :score chem97)))

