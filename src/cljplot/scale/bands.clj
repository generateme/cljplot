(ns cljplot.scale.bands
  (:require [fastmath.core :as m]
            [cljplot.scale.common :as sc]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- bands-inverse-fn
  "Inverse function for bands."
  [bands lst]
  (fn [^double v]
    (loop [s (map vector bands lst)]
      (when (seq s)
        (let [[band-id {:keys [^double start ^double end]}] (first s)]
          (if (<= start v end) band-id (recur (next s))))))))

(defn bands
  "Creates sequence of bands for given range and padding.

  Bands are evenly distributed intervals with padding.

  Each band is a map with following keys:

  * start - interval start
  * end - interval end
  * point - selected point (default: midpoint)

  Input parameters are:

  * bands - number of the bands (default: 1) or sequence of values
  * padding-in - padding between bands (default: 0.0)
  * padding-out - border padding (default: 0.0)
  * align - position of the selected point (0.0 - left, 1.0 - right, 0.5 - midpoint, default)

  Padding is calculated the same way as in `d3`. It's a proportion of the step."
  ([] (bands 1))
  ([b] (bands b {}))
  ([b {:keys [^double padding-in ^double padding-out ^double align]
       :or {padding-in 0.0 padding-out 0.0 align 0.5}}]
   (let [[bands-no bands] (if (sequential? b)
                            [(count b) b]
                            [b (range b)])
         bands-no (int bands-no)
         padding-in (m/constrain ^double padding-in 0.0 1.0)
         align (m/constrain ^double align 0.0 1.0)
         step (/ (+ (* bands-no (- 1.0 padding-in))
                    (+ padding-out padding-out)
                    (* (dec bands-no) padding-in)))
         nstart (* step padding-out)
         size (* step (- 1.0 padding-in))
         lst (for [^long i (range bands-no)
                   :let [lstart (+ nstart (* i step))
                         lend (+ lstart size)
                         [lstart lend] (if (neg? step) [lend lstart] [lstart lend])]]
               {:start lstart
                :end lend
                :point (m/lerp lstart lend align)})]
     (sc/->OrdinalScale bands lst bands-no :bands
                        (zipmap bands lst)
                        (bands-inverse-fn bands lst) {:bandwidth (m/abs size)
                                                      :step (m/abs step)}))))


(defmethod sc/ticks :bands [s & [c]]
  (sc/ordinal-ticks s c))

