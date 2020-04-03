(ns cljplot.scale.linear
  (:require [fastmath.core :as m]
            [fastmath.interpolation :as i]
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
            (sc/interpolated-range i/linear-smile :linear xs {:steps xs})))))

(defn- tick-increment
  ^double [^double start ^double end ^long count]
  (let [step (/ (- end start) (max 0 count))
        power (m/floor (m/log10 step))
        p10 (m/pow 10.0 power)
        error (/ step p10)]
    (if (>= power 0.0)
      (* p10 (sc/step-mult error))
      (/ (- (m/pow 10.0 (- power))) (sc/step-mult error)))))

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
        c (or c 10)
        accuracy (sc/tick-accuracy start end c)]
    (map #(+ 0.0 (m/approx % accuracy)) (ticks-linear start end c))))
