(ns cljplot.scale.discrete
  (:require [fastmath.core :as m]
            [fastmath.interpolation :as i]
            [fastmath.stats :as stats]
            [cljplot.scale.common :as sc]
            [clojure.set :refer [difference]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- interval-steps-before
  "Maps `steps` values into ordinal values (0,1,2...)."
  [steps]
  (comp int (i/step-before steps (range (count steps)))))

(defn- quantile-inverse
  [xs r]
  (fn [^double q]
    (reduce (fn [prev {:keys [^double start ^double end ^double quantile]}]
              (let [vs (difference
                        (set (filter (fn [^double v]
                                       (<= start v end)) xs)) prev)]
                (if (<= q quantile)
                  (reduced vs)
                  vs))) nil r)))

(defn quantile
  "Returns discrete scale for evenly distributed quantiles."
  ([xs] (quantile xs 10))
  ([xs steps-no] (quantile xs steps-no :legacy))
  ([xs ^long steps-no estimation-strategy]
   (let [xs (m/seq->double-array xs)
         [start end] (stats/extent xs)
         quantiles (rest (m/slice-range (inc steps-no)))
         steps (stats/quantiles xs quantiles estimation-strategy)
         steps-corr (conj (seq steps) start)
         r (mapv (fn [[^double x1 ^double x2] q id]
                   {:start x1
                    :end x2
                    :point (m/mlerp x1 x2 0.5)
                    :id id
                    :quantile q}) (partition 2 1 steps-corr) quantiles (range steps-no))]
     (sc/->DiscreteScale start end [start end] r steps-no :quantile
                         (comp r (interval-steps-before steps))
                         (quantile-inverse xs r) {:quantiles quantiles
                                                  :steps steps-corr}))))

(defn- linear-search
  [r]
  (fn [^double v]
    (reduce (fn [_ {:keys [start ^double end]}]
              (if (<= v end)
                (reduced [start end])
                [start end])) nil r)))

(defn- threshold-from-steps
  [steps]
  (if-not (sequential? steps)
    (threshold-from-steps (m/slice-range (inc ^long steps)))
    (let [[mn mx] (stats/extent steps)
          cnt (dec (count steps))
          r (mapv (fn [[^double x1 ^double x2] id]
                    {:start x1
                     :end x2
                     :id id
                     :point (m/mlerp x1 x2 0.5)}) (partition 2 1 steps) (range cnt))]
      (sc/->DiscreteScale mn mx [mn mx] r cnt :threshold
                          (comp r (interval-steps-before (rest steps)))
                          (linear-search r) {:steps steps}))))

(defn threshold
  "Returns discrete scale for given steps or evenly spliced domain."
  ([] (threshold-from-steps 10))
  ([steps] (if (sequential? steps)
             (threshold steps 10)
             (threshold-from-steps steps)))
  ([[start end] ^long steps-no] (threshold-from-steps (m/slice-range (inc steps-no) start end))))

(defmethod sc/ticks :quantile [s & [_]]
  (map :point (:range s)))

(defmethod sc/ticks :threshold [s & [_]]
  (map :point (:range s)))
