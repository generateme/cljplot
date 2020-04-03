(ns cljplot.utils
  "Common functions"
  (:require [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn slice-range 
  "Slice range to get `cnt` number of points evenly distanced."
  ([^long cnt ^double start ^double end] (if (= cnt 1)
                                           (list (+ start (* 0.5 (- end start))))
                                           (map #(m/mnorm % 0.0 (dec cnt) start end) (range cnt))))
  ([^long cnt] (slice-range cnt 0.0 1.0)))


