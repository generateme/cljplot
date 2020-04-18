(ns cljplot-new
  (:require [clojure.data.csv :as csv]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [tech.ml.dataset :as ds]
            [tech.ml.dataset.column :as col]))


(defn read-json [f] (with-open [reader (io/reader f)]
                      (doall (json/read-json reader true))))


(defn read-csv [f] (rest (with-open [reader (io/reader f)]
                           (doall (csv/read-csv reader)))))

(defn map-kv [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))


(def chem97 (ds/->dataset "data/chem97.json"))

(def lat1 (ds/group-by-column :score chem97))
(def lat2 (map-kv #(ds/select-columns % [:gcsescore]) lat1))

(def oats (ds/->dataset "data/oats.json"))

(ds/column-names oats)

(def lat1 (ds/group-by (juxt :Block :Variety) oats))
(def lat2 (map-kv #(ds/select-columns % [:nitro :yield]) lat1))

(def barley (ds/->dataset "data/barley.json"))

(def lat1 (ds/group-by-column :site barley))
(def lat2 (map-kv #(ds/group-by-column :year %) lat1))

(ds/mapseq-reader barley)


(get (get lat2 "Waseca") 1931)

(second lat2)

(keys lat1)

(ds/column chem97 :score)

(ds/column-names chem97)

(ds/->dataset (ds/mapseq-reader {:abba (repeatedly 100 rand)}))

(ds/name-values-seq->dataset {:abba (repeatedly 100 rand)
                              :df (repeat 100 10)
                              "sdf" (repeat 100 "s")})

(ds/group-by )

(seq (col/ (ds/column chem97 :score)))

(sort (distinct (map :score chem97)))

(keys (into (sorted-map) (group-by :score chem97)))

(def data (repeatedly 100 (comp m/approx r/grand)))

(m/co-intervals data 2 0.0)


(m/co-intervals [1 2 3 4 5 1 2 3 4] 4 0.0)


(defprotocol MyProtocol
  (callme [_]))

(defrecord TestMe [a b c]
  clojure.lang.IFn
  (invoke [_] a))

(extend-type TestMe
  MyProtocol
  (callme [t] (select-keys t [:a :b])))

(def test-me (->TestMe 11 "abc" :kkk))

(test-me);; => 11

(callme test-me)
;; => {:a 11, :b "abc"}
