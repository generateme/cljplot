(ns cljplot.impl.math
  (:require [cljplot.common :refer :all]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.color :as c]
            [fastmath.complex :as cx]
            [fastmath.random :as r]
            [fastmath.fields :as f]
            [clojure2d.core :refer :all]
            [fastmath.grid :as grid]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:private ^:const pi-r [m/-PI m/PI])

(defmethod data-extent :complex [_ _ {:keys [x y]}]
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

(defmethod render-graph :complex [_ f {:keys [colorspace permutation wrap-method] :as conf} {:keys [w h x y] :as chart-data}]
  (let [permutation (int permutation)
        iscale-x (:inverse (:scale x))
        iscale-y (:inverse (:scale y))
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
                ^double mag (cx/abs fv)
                mag (* 255.0 (wrap wrap-method mag))
                col (from-cs (permutation->color permutation angle (- 255.0 (/ (- 255.0 mag) 4.0)) mag))]
            (set-color c col)
            (rect c x y 1 1)))))))

;; scalar

(defmethod data-extent :scalar [_ d c] (data-extent :complex d c))

(defmethod render-graph :scalar [_ f {:keys [gradient wrap-method] :as conf} {:keys [w h x y] :as chart-data}]
  (let [iscale-x (:inverse (:scale x))
        iscale-y (:inverse (:scale y))
        dw (double w)
        dh (double h)]

    (do-graph chart-data false
      (dotimes [x w]
        (dotimes [y h]
          (let [xx (/ x dw)
                yy (/ y dh)
                sx (iscale-x xx)
                sy (iscale-y yy) 
                v (wrap wrap-method (f (v/vec2 sx sy)))]
            (set-color c (gradient v))
            (rect c x y 1 1)))))))

;; field

(defmethod prepare-data :field [_ f {:keys [x y points generator jitter wrap?]}]
  (let [[x1 x2] (or x pi-r)
        [y1 y2] (or y pi-r)
        f (if wrap? (comp (f/field :sinusoidal ) f) f)]
    (mapv (fn [[xx yy]] (f (v/vec2 (m/norm xx 0.0 1.0 x1 x2)
                                  (m/norm yy 0.0 1.0 y1 y2)))) (take points (r/jittered-sequence-generator generator 2 jitter)))))

(defmethod render-graph :field [_ data {:keys [color] :as conf} {:keys [^int w ^int h x y] :as chart-data}]
  (let [scale-x (:scale x)
        scale-y (:scale y)]

    (do-graph chart-data false
      (set-color c color) 
      (doseq [[xx yy] data]
        (rect c (* w ^double (scale-x xx)) (* h ^double (scale-y yy)) 1 1)))))

;; vectors

(defmethod data-extent :vector [_ f c] (data-extent :complex f c))

(defmethod render-graph :vector [_ f {:keys [^double size grid color ^double scale]} {:keys [^int w ^int h x y] :as chart-data}]
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

    (do-graph (assoc chart-data :oversize 0) true
      (set-color c color)
      (doseq [[x y] coords
              :let [xx (* w ^double (scale-x x))
                    yy (* h ^double (scale-y y))
                    v (f (v/vec2 x y))
                    len (* scale size (wrap :exp (v/mag v)))]] 
        (-> c
            (push-matrix)
            (translate xx yy)
            (ellipse 0 0 3 3 false)
            (rotate (v/heading v))
            (line 0 0 len 0))
        (when (> len 2.0)
          (line c len 0 (- len 2.0) -2.0)
          (line c len 0 (- len 2.0) 2.0))
        (pop-matrix c)))))

;;

(defmethod prepare-data :trace [_ f {:keys [x y points generator jitter]}]
  (let [[x1 x2] (or x pi-r)
        [y1 y2] (or y pi-r)]
    [f (mapv (fn [[xx yy]] (v/vec2 (m/norm xx 0.0 1.0 x1 x2)
                                  (m/norm yy 0.0 1.0 y1 y2))) (take points (r/jittered-sequence-generator generator 2 jitter)))]))

(defmethod data-extent :trace [_ data c] (data-extent :complex data c))

(defmethod render-graph :trace [_ [f coords] {:keys [^double step color ^double length]} {:keys [^int w ^int h x y] :as chart-data}]
  (let [scale-x (:scale x)
        scale-y (:scale y)]

    (do-graph (assoc chart-data :oversize 0) true
      (set-color c color)
      
      (doseq [v coords
              :let [p (take length
                            (iterate (fn [v]
                                       (let [nv (f v)]
                                         (v/add v (v/mult nv (* (wrap :exp (v/mag nv)) step))))) v))]]
        (doseq [[x y] p]
          (point c (* w ^double (scale-x x)) (* h ^double (scale-y y))))))))
