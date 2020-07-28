(ns cljplot.data
  (:require [tablecloth.api :as api]))

(defn data->dataset
  "Ensure dataset and pack into a map"
  [ds]
  {:dataset (api/dataset ds)})

(defn- data->groups-data
  "Group-by"
  [ds selector nms]
  (let [groups (-> ds                   
                   :dataset
                   (api/group-by selector))]
    {nms (:name groups)
     :dataset groups}))

(defn dataset->trellis
  "Group-by to prepare trellis chart"
  [ds selector]
  (-> ds
      (data->groups-data selector :trellis-names)
      (update :dataset vary-meta assoc :trellis? true)))

(defn dataset->groups
  "Group-by to prepare multi chart"
  [ds selector]
  (data->groups-data ds selector :group-names))

(defn dataset->trellis+groups
  "Group-by to prepare trellis with grouped charts"
  [ds trellis-selector groups-selector]
  (let [groups (dataset->groups ds groups-selector)
        trellis (dataset->trellis ds trellis-selector)]
    (assoc trellis
           :group-names (:group-names groups)
           :dataset (-> (:dataset trellis)
                        (api/unmark-group)
                        (api/map-columns :data #(api/group-by % groups-selector))))))

(defn ->dataset
  "Prepare dataset data with trellis/grouping information"
  [{:keys [data trellis-by groups-by]}]
  (let [ds (data->dataset data)]
    (cond
      (and trellis-by groups-by) (dataset->trellis+groups ds trellis-by groups-by)
      trellis-by (dataset->trellis ds trellis-by)
      groups-by (dataset->groups ds groups-by)
      :else ds)))

;;

(def chem97 (api/dataset "data/chem97.json"))

;; 1.1

(->dataset {:data chem97
            :trellis-by :score})

;; 1.3

(->dataset {:data chem97
            :groups-by :score})

;;

(def oats (api/dataset "data/oats.json"))

;; 2.1

;; TODO: Sorting keys
(->dataset {:data oats
            :trellis-by [:Block :Variety]})

;;

(def barley (api/dataset "data/barley.json"))

;; 2.6

(->dataset {:data barley
            :trellis-by :site
            :groups-by :year})

;; 2.7

;; TODO: Sorting keys
(->dataset {:data oats
            :trellis-by :Block
            :groups-by :Variety})

;;

(def titanic (api/dataset "data/titanic.json"))

;; 2.8

(->dataset {:data titanic
            :trellis-by [:Age :Sex]
            :groups-by :Survived})

;;

(def gvhd10 (api/dataset "data/gvhd10.json"))

;; 3.3

(->dataset {:data gvhd10
            :trellis-by :Days})

;; 3.6

(->dataset {:data chem97
            :trellis-by :gender
            :groups-by :score})

;; 3.8

(->dataset {:data chem97
            :trellis-by :score
            :groups-by :gender})

;;

(def postdoc (api/dataset "data/postdoc.csv"))

;; 4.5

;; everything wrong - stacked bar chart :/

