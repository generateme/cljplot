(ns cljplot.impl.math
  (:require [cljplot.common :as common]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.color :as c]
            [fastmath.complex :as cx]
            [fastmath.random :as r]
            [fastmath.fields :as f]
            [clojure2d.core :as c2d]
            [fastmath.grid :as grid]
            [fastmath.stats :as stats]
            [cljplot.scale :as s])
  (:import [marchingsquares Algorithm]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:private pi-r [m/-PI m/PI])

(defmethod common/data-extent :complex [_ _ {:keys [x y]}]
  {:x [:numerical (or x pi-r)]
   :y [:numerical (or y pi-r)]})

(defmacro ^:private permutation->color
  [p a b c]
  `(case ~p
     0 (c/color ~a ~b ~c)
     1 (c/color ~a ~c ~b)
     2 (c/color ~b ~a ~c)
     3 (c/color ~b ~c ~a)
     4 (c/color ~c ~a ~b)
     5 (c/color ~c ~b ~a)
     (c/color ~a ~b ~c)))

(defn- wrap
  ^double [method ^double v]
  (case method
    :log2 (m/frac (m/log2 (inc v)))
    :log10 (m/frac (m/log10 (inc v)))
    :sin (m/norm (m/sin v) -1.0 1.0 0.0 1.0)
    :exp (- 1.0 (m/exp (- v)))
    :sigmoid (m/sigmoid v)
    (m/frac v)))

(defmethod common/render-graph :complex [_ f {:keys [colorspace permutation wrap-method]} {:keys [w h x y] :as chart-data}]
  (let [permutation (int permutation)
        iscale-x (:inverse (:scale x))
        iscale-y (:inverse (:scale y))
        dw (double w)
        dh (double h)
        from-cs (or (second (c/colorspaces* colorspace)) c/from-HSB*)]

    (common/do-graph chart-data false
                     (dotimes [x w]
                       (dotimes [y h]
                         (let [xx (/ x dw)
                               yy (/ y dh)
                               sx (iscale-x xx)
                               sy (iscale-y yy)
                               fv (f (v/vec2 sx sy))
                               angle (m/norm (cx/arg fv) m/-PI m/PI 0.0 255.0)
                               ^double mag (cx/abs fv)
                               mag (* 255.0 (wrap wrap-method mag))
                               col (from-cs (permutation->color permutation angle (- 255.0 (/ (- 255.0 mag) 4.0)) mag))]
                           (c2d/set-color c col)
                           (c2d/rect c x y 1 1)))))))

;; scalar

(defmethod common/data-extent :scalar [_ d c] (common/data-extent :complex d c))

(defmethod common/render-graph :scalar [_ f {:keys [gradient wrap-method]} {:keys [w h x y] :as chart-data}]
  (let [iscale-x (:inverse (:scale x))
        iscale-y (:inverse (:scale y))
        dw (double w)
        dh (double h)]

    (common/do-graph chart-data false
                     (dotimes [x w]
                       (dotimes [y h]
                         (let [xx (/ x dw)
                               yy (/ y dh)
                               sx (iscale-x xx)
                               sy (iscale-y yy) 
                               v (wrap wrap-method (f (v/vec2 sx sy)))]
                           (c2d/set-color c (gradient v))
                           (c2d/rect c x y 1 1)))))))

;;

(defmethod common/data-extent :function-2d [_ d c] (common/data-extent :complex d c))

(defmethod common/render-graph :function-2d [_ f {:keys [gradient]} {:keys [^int w ^int h x y] :as chart-data}]
  (let [iscale-x (:inverse (:scale x))
        iscale-y (:inverse (:scale y))
        dw (double w)
        dh (double h)
        buffer (double-array (* w h))]

    (dotimes [y h]
      (let [off (* y w)]
        (dotimes [x w]
          (let [xx (/ x dw)
                yy (/ y dh)
                ^double v (f (iscale-x xx) (iscale-y yy))]
            (aset buffer (+ off x) v)))))

    (let [[mnz mxz] (stats/extent buffer)]
      (common/do-graph chart-data false
                       (dotimes [y h]
                         (let [off (* y w)]
                           (dotimes [x w]
                             (let [v (aget buffer (+ off x))]
                               (c2d/set-color c (gradient (m/norm v mnz mxz)))
                               (c2d/rect c x y 1 1)))))))))


;; contour

(defmethod common/data-extent :contour-2d [_ d c] (common/data-extent :complex d c))

(defmethod common/render-graph :contour-2d [_ f {:keys [palette ^int contours fill?]} {:keys [^int w ^int h x y] :as chart-data}]
  (let [palette (c/resample palette (inc contours))
        iscale-x (:inverse (:scale x))
        iscale-y (:inverse (:scale y))
        dw (double w)
        dh (double h)
        values (for [^long y (range h)
                     ^long x (range w)
                     :let [xx (/ x dw)
                           yy (/ y dh)]]
                 (f (iscale-x xx) (iscale-y yy)))
        ^Algorithm algo (Algorithm. (m/seq->double-double-array (partition (int w) values)))            
        steps (s/splice-range (inc contours) (.-min algo) (.-max algo))]
    (common/do-graph chart-data true
                     (doseq [[id p] (map-indexed vector (.buildContours algo (double-array steps)))
                             :let [col (nth palette id)]]
                       (if fill?
                         (do
                           (c2d/set-color c col)
                           (.fill ^java.awt.Graphics2D (.graphics ^clojure2d.core.Canvas c) p)
                           (c2d/set-color c (c/darken col))
                           (.draw ^java.awt.Graphics2D (.graphics ^clojure2d.core.Canvas c) p))
                         (do
                           (c2d/set-color c :black 200)
                           (.draw ^java.awt.Graphics2D (.graphics ^clojure2d.core.Canvas c) p)))))))


;; field

(defmethod common/prepare-data :field [_ f {:keys [x y points generator jitter wrap?]}]
  (let [[x1 x2] (or x pi-r)
        [y1 y2] (or y pi-r)
        f (if wrap? (comp (f/field :sinusoidal ) f) f)]
    (mapv (fn [[xx yy]] (f (v/vec2 (m/norm xx 0.0 1.0 x1 x2)
                                  (m/norm yy 0.0 1.0 y1 y2)))) (take points (r/jittered-sequence-generator generator 2 jitter)))))

(defmethod common/render-graph :field [_ data {:keys [color]} {:keys [^int w ^int h x y] :as chart-data}]
  (let [scale-x (:scale x)
        scale-y (:scale y)]

    (common/do-graph chart-data false
                     (c2d/set-color c color) 
                     (doseq [[xx yy] data]
                       (c2d/rect c (* w ^double (scale-x xx)) (* h ^double (scale-y yy)) 1 1)))))

;; vectors

(defmethod common/data-extent :vector [_ f c] (common/data-extent :complex f c))

(defmethod common/render-graph :vector [_ f {:keys [^double size grid color ^double scale]} {:keys [^int w ^int h x y] :as chart-data}]
  (let [scale-x (:scale x)
        scale-y (:scale y)
        iscale-x (:inverse scale-x)
        iscale-y (:inverse scale-y)
        grid (grid/grid grid size)
        hsize (/ size 2.0)
        coords (distinct (for [x (range 0 w hsize)
                               y (range 0 h hsize)
                               :let [[^double mx ^double my] (grid/coords->mid grid [x y])]]
                           [(iscale-x (/ mx w)) (iscale-y (/ my h))]))]

    (common/do-graph (assoc chart-data :oversize 0) true
                     (c2d/set-color c color)
                     (doseq [[x y] coords
                             :let [xx (* w ^double (scale-x x))
                                   yy (* h ^double (scale-y y))
                                   v (f (v/vec2 x y))
                                   len (* scale size (wrap :exp (v/mag v)))]] 
                       (-> c
                           (c2d/push-matrix)
                           (c2d/translate xx yy)
                           (c2d/ellipse 0 0 3 3 false)
                           (c2d/rotate (v/heading v))
                           (c2d/line 0 0 len 0))
                       (when (> len 2.0)
                         (c2d/line c len 0 (- len 2.0) -2.0)
                         (c2d/line c len 0 (- len 2.0) 2.0))
                       (c2d/pop-matrix c)))))

;;

(defmethod common/prepare-data :trace [_ f {:keys [x y points generator jitter]}]
  (let [[x1 x2] (or x pi-r)
        [y1 y2] (or y pi-r)]
    [f (mapv (fn [[xx yy]] (v/vec2 (m/norm xx 0.0 1.0 x1 x2)
                                  (m/norm yy 0.0 1.0 y1 y2))) (take points (r/jittered-sequence-generator generator 2 jitter)))]))

(defmethod common/data-extent :trace [_ data c] (common/data-extent :complex data c))

(defmethod common/render-graph :trace [_ [f coords] {:keys [^double step color ^double length]} {:keys [^int w ^int h x y] :as chart-data}]
  (let [scale-x (:scale x)
        scale-y (:scale y)]

    (common/do-graph (assoc chart-data :oversize 0) true
                     (c2d/set-color c color)
                     
                     (doseq [v coords
                             :let [p (take length
                                           (iterate (fn [v]
                                                      (let [nv (f v)]
                                                        (v/add v (v/mult nv (* (wrap :exp (v/mag nv)) step))))) v))]]
                       (doseq [[x y] p]
                         (c2d/point c (* w ^double (scale-x x)) (* h ^double (scale-y y))))))))
