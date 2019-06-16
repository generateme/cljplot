(ns cljplot.impl.heatmap
  (:require [cljplot.common :refer :all]
            [clojure2d.core :refer :all]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]
            [fastmath.stats :as stats]
            [fastmath.grid :as grid]
            [fastmath.vector :as v]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defmethod render-graph :cloud [_ data {:keys [kernel kernel-params] :as conf} {:keys [x y ^int w ^int h] :as chart-data}]
  (let [grad (if kernel
               (if kernel-params
                 (p/gradient-renderer w h kernel kernel-params)
                 (p/gradient-renderer w h kernel))
               (p/gradient-renderer w h))
        scale-x (:scale x)
        scale-y (:scale y)]
    (do-graph chart-data false

      (doseq [[x y] data]
        (p/add-pixel grad (* w ^double (scale-x x)) (* h ^double (scale-y y))))

      (let [p (p/to-pixels grad conf)]        
        (image c (get-image p) 0 0)))))

(defn- calc-bheatmap
  [data g sx sy]
  (reduce (fn [m [x y cnt]]
            (if-not (and x y)
              m
              (let [cnt (or cnt 1)
                    cell (grid/coords->mid g (v/vec2 (sx x) (sy y)))]
                (if (contains? m cell)
                  (update m cell clojure.core/+ cnt)
                  (assoc m cell cnt))))) {} data))

(defmethod render-graph :binned-heatmap [_ data {:keys [grid size gradient ^double alpha-factor] :as conf} {:keys [x y ^int w ^int h] :as chart-data}]
  (let [scale-x (partial (:scale x) 0 w)
        scale-y (partial (:scale y) 0 h)
        grid (grid/grid grid size)
        data (calc-bheatmap data grid scale-x scale-y)
        [mnz mxz] (stats/extent (vals data))
        gradient (if (pos? alpha-factor)
                   (fn [v] (let [id (m/norm v mnz mxz)]
                            (c/set-alpha (gradient id) (* 255.0 (m/pow id alpha-factor)))))
                   (fn [v] (gradient (m/norm v mnz mxz))))]
    (do-graph (assoc chart-data :oversize 0) false

      (doseq [[[x y] v] data]
        (set-color c (gradient v))
        (grid-cell c grid x y)))))

;;

(defmacro ^:private distinct-categorical
  [sort? data which]
  `(as-> (map ~which ~data) d#
     (distinct d#)
     (if ~sort? (sort d#) d#)))

(defmethod data-extent :heatmap [_ data {:keys [sort?]}]
  (let [ks (map first data)]
    {:x [:categorical (distinct-categorical sort? ks first)]
     :y [:categorical (distinct-categorical sort? ks second)]
     :z [:numerical (map second data)]}))

(defmethod render-graph :heatmap [_ data {:keys [gradient]} {:keys [x y w h] :as chart-data}]
  (let [scale-x (partial (:scale x) 0 w)]))
