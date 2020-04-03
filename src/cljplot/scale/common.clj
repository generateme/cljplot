(ns cljplot.scale.common
  (:require [fastmath.core :as m]
            [cljplot.utils :as u])
  (:import [clojure.lang IFn]
           [java.time LocalDateTime]))

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
   (let [target (u/slice-range (count xs))
         f (first xs)
         l (last xs)]
     (->ContinuousScale f l [f l] type (interpolator xs target) (interpolator target xs) info))))

;; ticks

(def ^:const ^:private ^double e10 (m/sqrt 50.0))
(def ^:const ^:private ^double e5 (m/sqrt 10.0))
(def ^:const ^:private ^double e2 m/SQRT2)

(defn step-mult
  "Step multiplier"
  ^double [^double error]
  (cond
    (>= error e10) 10.0
    (>= error e5) 5.0
    (>= error e2) 2.0
    :else 1.0))

(defn tick-step
  ^double [^double start ^double end ^long count]
  (let [step0 (/ (m/abs (- end start)) (max 0 count))
        step1 (m/pow 10 (m/floor (m/log10 step0)))
        error (/ step0 step1)
        ret (* step1 (step-mult error))]
    (if (< end start) (- ret) ret)))

(defn tick-accuracy
  ^long [^double start ^double end ^long count]
  (let [step (m/log10 (tick-step start end count))]
    (if (neg? step) (inc (m/ceil (m/abs step))) 4)))

(defn ordinal-ticks
  "Take nth tick from ordinal domain or return whole domain"
  [s c]
  (if c
    (take-nth (max 1 (int (m/floor (/ (count (:domain s)) (double c))))) (:domain s))
    (:domain s)))

(defmulti ticks (fn [s & _] (:type s)))
