(ns cljplot.impl.scatter
  (:require [clojure2d.core :as c2d]
            [cljplot.common :as common]
            [cljplot.scale :as s]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c])
  (:import [clojure2d.java.filter Blur]
           [marchingsquares Algorithm]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defmethod common/render-graph :scatter [_ data {:keys [color stroke size shape] :as conf}
                                         {:keys [w h x y] :as chart-data}]
  (let [scale-x (partial (:scale x) 0 w)
        scale-y (partial (:scale y) 0 h)]
    (common/do-graph chart-data (some #(#{\o \O} (shape % conf)) data)
                     (let [coords (mapv (fn [[x y]]
                                          (c2d/transform c (scale-x x) (scale-y y))) data)]
                       (c2d/reset-matrix c)
                       (doseq [[v [tx ty]] (map vector data coords)
                               :let [local-stroke (update stroke :size (:size stroke) v conf)]]
                         (common/draw-shape c tx ty (shape v conf) (color v conf) local-stroke (size v conf)))))))

(defmethod common/render-graph :bubble [_ data {:keys [size-range scale-z] :as conf} chart-data]
  (let [[mn mx] size-range
        z (:scale (s/scale-map scale-z {:domain (-> conf :extent :z second)}))
        size-fn (fn [[_ _ size] _] (z mn mx size))]
    (common/render-graph :scatter data (assoc conf :size size-fn) chart-data)))

(defmethod common/render-graph :gbubble [_ data {:keys [cells grid-type] :as conf} {:keys [x y] :as chart-data}]
  (let [scale-x (:scale x)
        scale-y (:scale y)
        hd (common/heatmap-grid data scale-x scale-y grid-type (or cells 10))
        z-extent (common/extent (map #(% 2) hd))]
    (common/render-graph :bubble hd (assoc-in conf [:extent :z] z-extent) chart-data)))

;;

(defmethod common/prepare-data :qqplot [_ [d1 d2] {:keys [points] :or {points 100}}]
  (let [d1 (if (r/distribution? d1) d1
               (r/distribution :real-discrete-distribution {:data (common/extract-first d1)}))
        d2 (if (r/distribution? d2) d2
               (r/distribution :real-discrete-distribution {:data (common/extract-first d2)}))]
    (map #(let [v (m/norm % 0 points)]
            (vector (r/icdf d1 v) (r/icdf d2 v))) (range 1 points))))

(defmethod common/render-graph :qqplot [_ data conf graph-conf] (common/render-graph :scatter data conf graph-conf))

(defmethod common/prepare-data :normal-plot [_ data conf] (common/prepare-data :qqplot [(r/distribution :normal) data] conf))
(defmethod common/render-graph :normal-plot [_ data conf graph-conf] (common/render-graph :scatter data conf graph-conf))

(defmethod common/prepare-data :ppplot [_ [d1 d2] {:keys [^int points domain] :or {points 100}}]
  (let [[dx dy] (or domain [-1.0 1.0])
        d1 (if (r/distribution? d1) d1
               (r/distribution :real-discrete-distribution {:data (common/extract-first d1)}))
        d2 (if (r/distribution? d2) d2
               (r/distribution :real-discrete-distribution {:data (common/extract-first d2)}))]
    (map #(let [v (m/norm % 0 points dx dy)]
            (vector (r/cdf d1 v) (r/cdf d2 v))) (range 0 (inc points)))))

(defmethod common/render-graph :ppplot [_ data conf graph-conf] (common/render-graph :scatter data conf graph-conf))

;;

(defonce ^:private bw-gradient (c/gradient [:black :white]))

(defmethod common/render-graph :density-2d [_ data {:keys [palette kernel kernel-params logarithmic? ^double blur-kernel-size ^int contours fill?]} {:keys [^int w ^int h x y] :as chart-data}]
  (let [palette (c/resample palette contours)
        scale-x (:scale x)
        scale-y (:scale y)
        g (if kernel
            (if kernel-params
              (p/gradient-renderer w h kernel kernel-params)
              (p/gradient-renderer w h kernel))
            (p/gradient-renderer w h))]

    (doseq [[x y] data]
      (p/add-pixel! g (* w ^double (scale-x x)) (* h ^double (scale-y y))))

    (let [g (->> (p/to-pixels g {:logarithmic? logarithmic? :gradient bw-gradient})
                 (map c/luma)
                 (m/seq->double-array))
          target (double-array (alength g))]

      (when (pos? blur-kernel-size)
        (Blur/gaussianBlur g target w h (if (< blur-kernel-size 1.0) (* 0.1 blur-kernel-size (max w h)) blur-kernel-size)))

      (let [^Algorithm algo (Algorithm. (m/seq->double-double-array (partition (int w) target)))            
            steps (rest (s/splice-range (inc contours) (.-min algo) (.-max algo)))]
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
                (.draw ^java.awt.Graphics2D (.graphics ^clojure2d.core.Canvas c) p)))))))))

(m/unuse-primitive-operators)
