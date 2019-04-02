(ns cljplot.impl.scatter
  (:require [clojure2d.core :refer :all]
            [cljplot.common :refer :all]
            [cljplot.scale :as s]
            [fastmath.stats :as stats]
            [fastmath.vector :as v]
            [fastmath.random :as r]
            [fastmath.core :as m]))


(defmethod render-graph :scatter [_ data {:keys [color stroke size shape] :as conf}
                                  {:keys [w h x y] :as chart-data}]
  (let [scale-x (partial (:scale x) 0 w)
        scale-y (partial (:scale y) 0 h)]
    (do-graph chart-data (some #(#{\o \O} (shape % conf)) data)
      (let [coords (mapv (fn [[x y]]
                           (transform c (scale-x x) (scale-y y))) data)]
        (reset-matrix c)
        (doseq [[v [tx ty]] (map vector data coords)
                :let [local-stroke (update stroke :size (:size stroke) v conf)]]
          (draw-shape c tx ty (shape v conf) (color v conf) local-stroke (size v conf)))))))

(defmethod render-graph :bubble [_ data {:keys [size-range scale-z] :as conf} chart-data]
  (let [[mn mx] size-range
        z (:scale (s/scale-map scale-z {:domain (-> conf :extent :z second)}))
        size-fn (fn [[_ _ size] _] (z mn mx size))]
    (render-graph :scatter data (assoc conf :size size-fn) chart-data)))

(defmethod render-graph :gbubble [_ data {:keys [cells grid-type] :as conf} {:keys [x y] :as chart-data}]
  (let [scale-x (:scale x)
        scale-y (:scale y)
        hd (heatmap-grid data scale-x scale-y grid-type (or cells 10))
        z-extent (extent (map #(% 2) hd))]
    (render-graph :bubble hd (assoc-in conf [:extent :z] z-extent) chart-data)))


;;

(defmethod prepare-data :ppplot [_ [d1 d2] {:keys [points] :or {points 100}}]
  (let [d1 (if (satisfies? r/DistributionProto d1) d1
               (r/distribution :empirical {:data (sort (extract-first d1))}))
        d2 (if (satisfies? r/DistributionProto d2) d2
               (r/distribution :empirical {:data (extract-first d2)}))]
    (map #(let [v (m/norm % 0 points)]
            (vector (r/icdf d1 v) (r/icdf d2 v))) (range 1 points))))

(defmethod render-graph :ppplot [_ data conf graph-conf] (render-graph :scatter data conf graph-conf))

(defmethod prepare-data :qqplot [_ data conf] (prepare-data :ppplot [(r/distribution :normal) data] conf))
(defmethod render-graph :qqplot [_ data conf graph-conf] (render-graph :scatter data conf graph-conf))
