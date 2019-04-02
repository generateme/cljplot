(ns cljplot.impl.math
  (:require [cljplot.common :refer :all]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.color :as c]
            [fastmath.complex :as cx]
            [clojure2d.core :refer :all]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defmethod data-extent :complex [_ _ {:keys [x y]}]
  {:x [:numerical (or x [m/-PI m/PI])]
   :y [:numerical (or y [m/-PI m/PI])]})

(defmethod render-graph :complex [_ f {:keys [colorspace] :as conf} {:keys [w h x y] :as chart-data}]
  (let [scale-x (:scale x)
        scale-y (:scale y)
        iscale-x (:inverse scale-x)
        iscale-y (:inverse scale-y)
        dw (double w)
        dh (double h)
        from-cs (or (second (c/colorspaces* colorspace)) c/from-HSB*)]

    (do-graph chart-data false

      (dotimes [x w]
        (dotimes [y h]
          (let [xx (/ x dw)
                yy (/ y dh)
                sx (iscale-x xx)
                sy (iscale-y yy)
                fv (f (v/vec2 sx sy))
                angle (m/norm (cx/arg fv) m/-PI m/PI 0.0 255.0)
                mag (* 255.0 (m/frac (m/log2 (+ 1.0 ^double (cx/abs fv)))))
                col (from-cs (c/color angle (- 255.0 (/ (- 255.0 mag) 4.0)) mag))]
            (set-color c col)
            (rect c x y 1 1)))))))



c/colorspaces*
