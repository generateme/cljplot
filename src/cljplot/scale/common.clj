(ns cljplot.scale.common
  (:require [fastmath.core :as m]
            [cljplot.scale :as s])
  (:import [clojure.lang IFn]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defrecord ContinuousScale [start end domain type forward inverse info]
  IFn
  (invoke [_ v] (forward v))
  (invoke [_ s e v] (m/mlerp ^double s ^double e ^double (forward v))))

(defrecord DiscreteScale [start end domain range cnt type forward inverse info]
  IFn
  (invoke [_ v] (forward v))
  (invoke [_ xs v] (nth xs (forward v))))

(defrecord OrdinalScale [domain range cnt type forward inverse info]
  IFn
  (invoke [_] range)
  (invoke [_ v] (forward v)))

(defn interpolated-range
  "Interpolate for ranges"
  ([interpolator type xs] (interpolated-range interpolator type xs nil))
  ([interpolator type xs info]
   (let [target (m/slice-range (count xs))
         f (first xs)
         l (last xs)]
     (->ContinuousScale f l [f l] type (interpolator xs target) (interpolator target xs) info))))

;; ticks

(defn ordinal-ticks
  "Take nth tick from ordinal domain or return whole domain"
  [s c]
  (if c
    (take-nth (max 1 (int (m/floor (/ (count (:domain s)) (double c))))) (:domain s))
    (:domain s)))

(defmulti ticks (fn [s & _] (:type s)))
