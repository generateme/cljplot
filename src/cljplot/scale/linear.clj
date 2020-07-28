(ns cljplot.scale.linear
  (:require [fastmath.core :as m]
            [fastmath.interpolation :refer [linear-smile]]
            [cljplot.scale.common :as sc]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn linear
  "Linear mapping function. Maps given domain to a [0,1] range."
  ([] (sc/->ContinuousScale 0.0 1.0 [0.0 1.0] :linear identity identity nil))
  ([xs] (if-not (or (nil? xs)
                    (sequential? xs))
          (linear [0.0 xs])
          (case (count xs)
            0 (linear)
            1 (linear [0.0 (first xs)])
            2 (let [[start end] xs]
                (sc/->ContinuousScale start end xs :linear
                                      (m/make-norm start end 0.0 1.0) (partial m/lerp start end) nil))
            (sc/interpolated-range linear-smile :linear xs {:steps xs})))))

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

(defn- tick-increment
  ^double [^double start ^double end ^long count]
  (let [step (/ (- end start) (max 0 count))
        power (m/floor (m/log10 step))
        p10 (m/pow 10.0 power)
        error (/ step p10)]
    (if (>= power 0.0)
      (* p10 (step-mult error))
      (/ (- (m/pow 10.0 (- power))) (step-mult error)))))

(defn ticks-linear
  [^double start ^double end ^long count]
  (if (and (== start end) (pos? count))
    [start]
    (let [reverse? (< end start)
          [^double start ^double end] (if reverse? [end start] [start end])
          step (tick-increment start end count)
          res (cond
                (or (zero? step)
                    (Double/isInfinite step)) []
                (pos? step) (let [start (m/ceil (/ start step))
                                  end (m/floor (/ end step))
                                  n (m/ceil (inc (- end start)))]
                              (map #(* (+ start ^double %) step) (range n)))
                :else (let [start (m/floor (* start step))
                            end (m/ceil (* end step))
                            n (m/ceil (inc (- start end)))]
                        (map #(/ (- start ^double %) step) (range n))))]
      (if reverse? (reverse res) res))))

(defmethod sc/ticks :default [s & [c]]
  (let [start (:start s)
        end (:end s)
        c (or c 10)]
    (ticks-linear start end c)))
