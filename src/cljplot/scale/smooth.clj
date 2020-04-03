(ns cljplot.scale.smooth
  (:require [fastmath.core :as m]
            [fastmath.interpolation :as i]
            [fastmath.easings :as e]
            [cljplot.scale.common :refer [interpolated-range]]
            [cljplot.scale.linear :refer [linear]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- find-interpolator
  "Create interpolator function."
  [easing? interpolator]
  (if easing?
    (let [easing (if (or (nil? interpolator) (keyword? interpolator))
                   (get e/easings-list interpolator e/linear)
                   interpolator)]
      (fn [[^double dx ^double dy] [^double rx ^double ry]]
        (let [n (m/make-norm dx dy 0.0 1.0)]
          (comp (partial m/lerp rx ry) easing n))))
    (if (or (nil? interpolator) (keyword? interpolator))
      (get i/interpolators-1d-list interpolator :monotone)
      interpolator)))

(defn smooth
  "Smooth mapping function.

  If xs has two elements, easing interpolation can be used (linear by default).
  If xs has more than two elements, sliced interpolation is used."
  ([] (linear))
  ([xs] (smooth xs nil))
  ([xs interpolator]
   (cond
     (not (sequential? xs)) (linear [0.0 xs])
     (not (seq xs)) (linear)
     (m/one? (count xs)) (linear [0.0 (first xs)])
     :else (let [interpolator-method (find-interpolator (= (count xs) 2) interpolator)]
             (interpolated-range interpolator-method :smooth xs {:interpolator interpolator})))))
