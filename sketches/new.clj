(ns cljplot-new
  (:require [clojure.data.csv :as csv]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [tech.ml.dataset :as ds]
            [tech.ml.dataset.column :as col]
            [tech.v2.datatype.functional :as dfn]))


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


;;

;; TODO: read from URL

(def flights (ds/->dataset (.openStream (java.net.URL. "https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv"))))

;; TODO: set ds name as filename

(meta flights) ;; => {:name nil}

;; TODO: add dim (or put dimensions into metadata

(ds/column-count flights) ;; => 11
(ds/row-count flights) ;; => 253316

(def DT (ds/name-values-seq->dataset {:ID [:b :b :b :c :c :a]
                                      :a (range 1 7)
                                      :b (range 7 13)
                                      :c (range 13 19)}))



(meta (DT :a))
;; => {:name :a, :size 6, :datatype :int64}

(:datatype (meta (ds/column DT :a)))
;; => :int64

;; TODO: head/tail for rows

(-> (ds/filter #(and (= (get % "origin") "JFK")
                     (= (get % "month") 6)) flights)
    (ds/select-rows (range 6)))
;; => null [6 11]:

| year | month | day | dep_delay | arr_delay | carrier | origin | dest | air_time | distance | hour |
|------+-------+-----+-----------+-----------+---------+--------+------+----------+----------+------|
| 2014 |     6 |   1 |        -9 |        -5 |      AA |    JFK |  LAX |      324 |     2475 |    8 |
| 2014 |     6 |   1 |       -10 |       -13 |      AA |    JFK |  LAX |      329 |     2475 |   12 |
| 2014 |     6 |   1 |        18 |        -1 |      AA |    JFK |  LAX |      326 |     2475 |    7 |
| 2014 |     6 |   1 |        -6 |       -16 |      AA |    JFK |  LAX |      320 |     2475 |   10 |
| 2014 |     6 |   1 |        -4 |       -45 |      AA |    JFK |  LAX |      326 |     2475 |   18 |
| 2014 |     6 |   1 |        -6 |       -23 |      AA |    JFK |  LAX |      329 |     2475 |   14 |


(ds/select-rows flights (range 2));; => null [2 11]:

| year | month | day | dep_delay | arr_delay | carrier | origin | dest | air_time | distance | hour |
|------+-------+-----+-----------+-----------+---------+--------+------+----------+----------+------|
| 2014 |     1 |   1 |        14 |        13 |      AA |    JFK |  LAX |      359 |     2475 |    9 |
| 2014 |     1 |   1 |        -3 |        13 |      AA |    JFK |  LAX |      363 |     2475 |   11 |


;; TODO: sort by columns and order 

(-> (ds/sort-by #(vector (get % "origin")
                         (get % "dest"))
                (fn [[^String o1 ^String d1] [^String o2 ^String d2]]
                  (let [compare-first (.compareTo o1 o2)]
                    (if-not (zero? compare-first)
                      compare-first
                      (- (.compareTo d1 d2))))) flights)
    (ds/select-rows (range 6)))
;; => null [6 11]:

| year | month | day | dep_delay | arr_delay | carrier | origin | dest | air_time | distance | hour |
|------+-------+-----+-----------+-----------+---------+--------+------+----------+----------+------|
| 2014 |     6 |   3 |        -6 |       -38 |      EV |    EWR |  XNA |      154 |     1131 |    6 |
| 2014 |     1 |  20 |        -9 |       -17 |      EV |    EWR |  XNA |      177 |     1131 |    8 |
| 2014 |     3 |  19 |        -6 |        10 |      EV |    EWR |  XNA |      201 |     1131 |    6 |
| 2014 |     2 |   3 |       231 |       268 |      EV |    EWR |  XNA |      184 |     1131 |   12 |
| 2014 |     4 |  25 |        -8 |       -32 |      EV |    EWR |  XNA |      159 |     1131 |    6 |
| 2014 |     2 |  19 |        21 |        10 |      EV |    EWR |  XNA |      176 |     1131 |    8 |


(flights "arr_delay")
;; => #tech.ml.dataset.column<int16>[253316]
arr_delay
[13, 13, 9, -26, 1, 0, -18, -14, -17, -14, -17, -5, 1, 133, -26, 69, 36, 1, 185, -6, ...]

(ds/select flights ["arr_delay"] (range 6))
;; => null [6 1]:

| arr_delay |
|-----------|
|        13 |
|        13 |
|         9 |
|       -26 |
|         1 |
|         0 |


;; TODO: consider to add more columns or a map to select and rename columns at the same time

(-> (ds/select-columns flights ["arr_delay" "dep_delay"])
    (ds/rename-columns  {"arr_delay" "delay_arr"
                         "dep_delay" "delay_dep"})
    (ds/select-rows (range 6)))
;; => null [6 2]:

| delay_arr | delay_dep |
|-----------+-----------|
|        13 |        14 |
|        13 |        -3 |
|         9 |         2 |
|       -26 |        -8 |
|         1 |         2 |
|         0 |         4 |

;;

(-> (ds/filter #(neg? (+ (get % "arr_delay")
                         (get % "dep_delay"))) flights)
    (ds/row-count))
;; => 141814

;; TODO: how to get ds instead of simple values?

(->> (-> (ds/filter #(and (= (get % "origin") "JFK")
                          (= (get % "month") 6)) flights)
         (ds/select-columns ["arr_delay" "dep_delay"]))
     (map dfn/mean))
;; => (5.839349323200929 9.807884113037279)


(ds/row-count (ds/filter #(and (= (get % "origin") "JFK")
                               (= (get % "month") 6)) flights))
;; => 8422

(defn map-kv [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))

;; TODO: select all columns but not provided

;; Aggregation

;; TODO: would be great to get dataset

(->> flights
     (ds/group-by-column "origin")
     (map-kv ds/row-count))
;; => {"EWR" 87400, "LGA" 84433, "JFK" 81483}

(->> flights
     (ds/group-by-column "origin")
     (map-kv (comp vector ds/row-count))
     (ds/name-values-seq->dataset))
;; => _unnamed [1 3]:

|       EWR |       LGA |       JFK |
|-----------+-----------+-----------|
| 8.740E+04 | 8.443E+04 | 8.148E+04 |


;; TODO: fix notation maybe?

(->> flights
     (ds/filter #(= "AA" (get % "carrier")))
     (ds/group-by-column "origin")
     (map-kv (comp vector ds/row-count))
     (ds/name-values-seq->dataset))
;; => _unnamed [1 3]:

|  EWR |       LGA |       JFK |
|------+-----------+-----------|
| 2649 | 1.173E+04 | 1.192E+04 |


;; TODO: how to convert it back to dataset?

(->> flights
     (ds/filter #(= "AA" (get % "carrier")))
     (ds/group-by (fn [r] (vector (get r "origin")
                                 (get r "dest"))))
     (map-kv ds/row-count))

;; 

(->> flights
     (ds/filter #(= "AA" (get % "carrier")))
     (ds/group-by (fn [r] (vector (get r "origin")
                                 (get r "dest")
                                 (get r "month"))))
     (map-kv #(->> (ds/select-columns % ["arr_delay" "dep_delay"])
                   (map dfn/mean))))

