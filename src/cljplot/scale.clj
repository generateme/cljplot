(ns cljplot.scale
  (:require [fastmath.core :as m]
            [java-time :as dt]
            [cljplot.scale.common :as sc]

            [cljplot.scale.linear :refer [linear]]
            [cljplot.scale.log :refer [log]]
            [cljplot.scale.pow :refer [pow]]
            [cljplot.scale.time :refer [time-scale time-format]]
            [cljplot.scale.bands :refer [bands]]
            [cljplot.scale.smooth :refer [smooth]]
            [cljplot.scale.ordinal :refer [ordinal]]
            [cljplot.scale.discrete :refer [quantile threshold]])
  (:import [cljplot.scale.common ContinuousScale DiscreteScale OrdinalScale]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn inverse
  "Return inverse scale value ([0,1]->domain). If not available, return `nil`."
  [s v]
  (when-let [inv (:inverse s)] (inv v)))

(defn continuous? [s] (instance? ContinuousScale s))
(defn discrete? [s] (instance? DiscreteScale s))
(defn ordinal? [s] (instance? OrdinalScale s))

;; scale/ticks factory

(def ^:private scales
  {:linear linear
   :smooth smooth
   :log log
   ;; :log1p log1p
   :pow pow
   :time time-scale
   :bands bands
   :ordinal ordinal
   :quantile quantile
   :threshold threshold})

(def ^:private default-domains
  {:linear [0.0 1.0]
   :smooth [0.0 1.0]
   :pow [0.0 1.0]
   :log [1.0 10.0]
   ;; :log1p [0.0 9.0]
   :time [(dt/minus (dt/local-date-time) (dt/years 1))
          (dt/local-date-time)]
   :bands 1
   :ordinal [0]
   :quantile [0]
   :threshold 10})

(defn scale-def->scale
  ([scale-def] (scale-def->scale scale-def nil))
  ([[scale-name & r] domain]
   (let [domain (or domain (default-domains scale-name))]
     {:scale (apply (scales scale-name) domain r)
      :domain domain})))

(defn scale->ticks
  ([scale] (scale->ticks scale nil))
  ([scale ticks]
   (if-not (sequential? ticks)
     {:ticks (sc/ticks scale ticks)}
     {:ticks ticks})))

(defn- coerce-format-fn
  "Find formatting function."
  ([scale fmt] (coerce-format-fn scale nil fmt))
  ([scale ticks fmt]
   (cond
     (string? fmt) (partial (if (= (:type scale) :time) dt/format format) fmt)
     (fn? fmt) fmt
     (= (:type scale) :time) (time-format scale ticks)
     :else str)))

(defn scale-map
  "Create scale data from definition"
  ([scale-def] (scale-map scale-def nil))
  ([scale-def {tick-values :ticks
               domain :domain
               ofmt :fmt}]
   (let [{:keys [domain scale]} (scale-def->scale scale-def domain)
         {:keys [ticks]} (scale->ticks scale tick-values)]
     {:domain domain
      :fmt (coerce-format-fn scale ticks ofmt)
      :ticks ticks
      :scale scale
      :scale-def scale-def})))

(defn- scale-type-from-map
  [scale-map]
  (:type (:scale scale-map)))

(defn update-scale
  [sc & [field value & r]]
  (let [new-sc (case field
                 :domain (-> (scale-map (:scale-def sc) {:domain value})
                             (assoc :fmt (:fmt sc))) ;; recalculate everything, keep format
                 :fmt (assoc sc :fmt (coerce-format-fn (scale-type-from-map sc) value))
                 :scale (scale-map value (dissoc sc :ticks)) ;; recalculate ticks
                 :ticks (merge sc (scale->ticks (:scale sc) value))
                 sc)]
    (if (seq r) (apply update-scale new-sc r) new-sc)))
