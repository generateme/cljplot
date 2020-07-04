(ns testing.dataset
  (:require [tech.ml.dataset :as ds]
            [tech.ml.dataset.column :as col]
            [tech.v2.datatype.functional :as dfn]))

;; Working thrugh R `data.table` type and confronting with tech.ml.dataset
;; https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html

;; # Read data from URL

;; TODO: add support for loading from the net (issue: #33)

(def flights (ds/->dataset (.openStream (java.net.URL. "https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv"))))

;; # Taking the shape of loaded data

(ds/column-count flights) ;; => 11
(ds/row-count flights) ;; => 253316

;; TODO: maybe add those numbers to a metadata? Like in `column` case?
;; TODO: also, maybe add automatically name from filename when file/URL was used to create dataset
;; TODO: column names as keywords during loading dataset.

(meta flights) ;; => {:name nil}

;; # Basics

(def DT (ds/name-values-seq->dataset {:ID [:b :b :b :c :c :a]
                                      :a (range 1 7)
                                      :b (range 7 13)
                                      :c (range 13 19)}))

(meta (DT :a))
;; => {:name :a, :size 6, :datatype :int64}

(:datatype (meta (ds/column DT :a)))
;; => :int64

;; # Subset rows

;; TODO: maybe add head/tail for rows instead of calling `select-rows`

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


;; # Sort by 2 columns and given order

;; TODO: below I want to sort dataset by origin ascending and dest descending. Maybe such case should be
;;       wrapped into the function? Writing comparators is not convinient in this quite common case.

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

;; # Select column


;; returns column
(flights "arr_delay")
;; => #tech.ml.dataset.column<int16>[253316]
arr_delay
[13, 13, 9, -26, 1, 0, -18, -14, -17, -14, -17, -5, 1, 133, -26, 69, 36, 1, 185, -6, ...]

;; returns dataset
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

;; TODO: question: consider IFn returns dataset by default. Arguments:
;; * column name - returns dataset with single column
;; * sequence of columns - returns dataset with selected columns
;; * map - returns dataset with selected and renamed columns

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

;; TODO: maybe ds should be also countable?

(-> (ds/filter #(neg? (+ (get % "arr_delay")
                         (get % "dep_delay"))) flights)
    (ds/row-count))
;; => 141814

;; # Functions on columns

;; TODO: I would prefer to get dataset here

(->> (-> (ds/filter #(and (= (get % "origin") "JFK")
                          (= (get % "month") 6)) flights)
         (ds/select-columns ["arr_delay" "dep_delay"]))
     (map dfn/mean))
;; => (5.839349323200929 9.807884113037279)

;; # Aggregation

(defn map-kv [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))

;; TODO: maybe add `group-by-and-aggregate` which returns dataset after group-by and aggregation?

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


;; TODO: Scientific notation should be used for bigger numbers

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

;; below: select, group by more than one column and aggregate (row-count)

(->> flights
     (ds/filter #(= "AA" (get % "carrier")))
     (ds/group-by (fn [r] (vector (get r "origin")
                                 (get r "dest"))))
     (map-kv ds/row-count)
     (take 6))
;; => ([["JFK" "BOS"] 1173]
;;     [["JFK" "DCA"] 172]
;;     [["EWR" "PHX"] 121]
;;     [["LGA" "PBI"] 245]
;;     [["EWR" "LAX"] 62]
;;     [["JFK" "SJU"] 690])

;; below: select, group-by 3 columns and aggregate 2 columns

(->> flights
     (ds/filter #(= "AA" (get % "carrier")))
     (ds/group-by (fn [r] (vector (get r "origin")
                                 (get r "dest")
                                 (get r "month"))))
     (map-kv #(->> (ds/select-columns % ["arr_delay" "dep_delay"])
                   (map dfn/mean)))
     (take 6))
;; => ([["LGA" "ORD" 10] (15.46611909650922 12.911704312114958)]
;;     [["EWR" "PHX" 9] (-4.2333333333333325 -1.6666666666666665)]
;;     [["JFK" "AUS" 3] (8.193548387096774 2.70967741935484)]
;;     [["JFK" "BOS" 2] (11.179999999999996 11.760000000000005)]
;;     [["LGA" "DFW" 10] (3.5 4.5527638190954605)]
;;     [["JFK" "BOS" 10] (11.5 10.2704918032787)])


(ds/->dataset "https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv")

