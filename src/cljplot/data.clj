(ns cljplot.data
  (:require [tech.ml.dataset :as ds]
            [tech.ml.dataset.column :as col]
            [tech.v2.datatype.datetime.operations :as dt-ops]
            [clojure.java.io :as io]))

(def chem97 (ds/->dataset "data/chem97.json"))

;; 1.1
;; data chem97
;; trellis by :score
;; no groups
;; plot histogram/density :gcsescore
;; color - fixed

(def l (ds/group-by-column :score chem97))
;; x
((first (vals l)) :gcsescore)
;; trellis panels:
(keys l)


;; 1.3

;; data chem97
;; no trellis
;; groups by :score
;; plot :gcsescore
;; groups layers:
;; color; palette
(keys l)

(def oats (ds/->dataset "data/oats.json"))

;; 2.1

;; data oats
;; trellis by [:block :variety]

(def l (ds/group-by identity oats [:Block :Variety]))
;; x
((first (vals l)) :nitro)
;; y
((first (vals l)) :yield)

;; trellis panels:
(sort-by (juxt :Block :Variety) (keys l))


;;

(def barley (ds/->dataset "data/barley.json"))

;; data barley
;; trellis by :site
(def l (ds/group-by-column :site barley))

;; trellis panels
(sort (keys l))
;; => ("Crookston" "Duluth" "Grand Rapids" "Morris" "University Farm" "Waseca")

;; groups by year
(def g (ds/group-by-column :year (first (vals l))))

(sort (keys g))

;; labels
(keys (ds/group-by-column :year barley))
;; => (1931 1932)

;; x
((first (vals g)) :yield)
;; y
((first (vals g)) :variety)

(map meta (ds/columns barley))
;; => ({:name :yield, :size 120, :datatype :object}
;;     {:categorical? true, :name :variety, :size 120, :datatype :string}
;;     {:name :year, :size 120, :datatype :int64}
;;     {:categorical? true, :name :site, :size 120, :datatype :string})


(->> (ds/add-or-update-column d "month" (dt-ops/get-months (d "date")))
     (ds/group-by-column "month"))


(ds/group-by #(dt-ops/get-months (get % "date")) d)


(transpose chem97 [:lea :student])


(defn transpose [ds col-names-seq]
  (let [size (ds/row-count ds)]
    (reduce ds/concat (map (fn [col-name]
                             (let [data (ds col-name)]
                               (ds/new-dataset
                                [(col/new-column :column (repeat (count data) col-name))
                                 (col/set-name data :value)]))) col-names-seq))))

(ds/concat (-> (ds/select-columns DT [:ID :a])
               (ds/rename-columns {:a :val}))
           (-> (ds/select-columns DT [:ID :b])
               (ds/rename-columns {:b :val})))


(reduce ds/concat (map (fn [col-name]
                         (let [data (chem97 col-name)]
                           (ds/new-dataset
                            [(col/new-column :column (repeat (count data) col-name))
                             (col/set-name data :value)]))) [:gender :lea]))

(reduce + (chem97 :gcsescore))
;; => 194994.49800000832

(reduce + (map int (ds/columns chem97)))
;; => ({:categorical? true, :name :lea, :size 31022, :datatype :string} {:categorical? true, :name :school, :size 31022, :datatype :string} {:categorical? true, :name :student, :size 31022, :datatype :string} {:name :score, :size 31022, :datatype :int64} {:categorical? true, :name :gender, :size 31022, :datatype :string} {:name :age, :size 31022, :datatype :int64} {:name :gcsescore, :size 31022, :datatype :object} {:name :gcsecnt, :size 31022, :datatype :float64})

(chem97 :gcsescore)
;; => #tech.ml.dataset.column<object>[31022]
:gcsescore
[6.625, 7.625, 7.250, 7.500, 6.444, 7.750, 6.750, 6.909, 6.375, 7.750, 7.857, 7.333, 7.750, 7.700, 6.300, 7.300, 6.636, 7.272, 7.200, 6.454, ...]

(transpose chem)

(count (chem97 :gcsecnt))

(-> [{:a 1 :b 2 :c 3} {:a 4 :b 5 :c 6}]
    (ds/->dataset)
    (transpose [:a :b :c]))
;; => null [6 2]:
;;    | :column | :value |
;;    |---------+--------|
;;    |      :a |      1 |
;;    |      :a |      4 |
;;    |      :b |      2 |
;;    |      :b |      5 |
;;    |      :c |      3 |
;;    |      :c |      6 |


(def vadeaths (ds/->dataset "data/VADeaths.csv"))

(map type (apply vector '(int 2 3)))
;; => (clojure.lang.Symbol java.lang.Long java.lang.Long)

(apply vector (list int 2 3))
;; => [#function[clojure.core/int] 2 3]
