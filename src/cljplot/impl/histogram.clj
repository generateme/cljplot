(ns cljplot.impl.histogram
  (:require [cljplot.common :refer :all]
            [cljplot.scale :as s]
            [fastmath.core :as m]
            [fastmath.stats :as stats]
            [clojure2d.core :refer :all]
            [clojure2d.color :as c]))

(defn- swap-counts
  [h]
  (update h :bins #(map (fn [[x y1 y2]] [x y2 y1]) %)))

(defmethod prepare-data :histogram [_ data {:keys [bins percents?]}]
  (let [s? (sequential? (first data))
        fd (if s? (flatten data) data)
        b (stats/estimate-bins fd bins)
        e (stats/extent fd)
        hs (map (fn [d]
                  (let [h (stats/histogram d b e)]
                    (if percents? (swap-counts h) h))) (if s? data [data]))
        b (map first (:bins (first hs)))
        cnts (apply map vector (map #(map second (:bins %)) hs))]
    (-> (select-keys (first hs) [:min :max :size :step])
        (assoc :samples (map :samples hs))
        (assoc :bins (map vector b cnts)))))

(defmethod data-extent :histogram [_ data _]
  (let [d (map second (:bins data))        
        max-bin (reduce max (flatten d))
        mn (:min data)
        mx (:max data)
        diff (- mx mn)]
    {:x [:numerical [(if (< diff 2.0) mn (m/floor mn))
                     (if (< diff 2.0) mx (m/ceil mx))]]
     :y [:numerical [0 max-bin]]}))

(defn- draw-rectangles
  [canvas {:keys [palette stroke? stroke]} bands w ys scale-y]
  (let [zero+ (inc (scale-y 0))]
    (doseq [[id y] (map-indexed vector ys)
            :let [{:keys [start end point]} (bands id)
                  x (* w start)
                  ww (* w (- end start))
                  col (nth palette id)
                  yy (- (scale-y y) 4)]]
      (if stroke?
        (-> canvas
           (set-stroke-custom stroke)
           (filled-with-stroke col (c/darken col) rect x zero+ ww yy))
        (-> canvas
           (set-color col) 
           (rect x zero+ ww yy)))))
  canvas)

(defn- draw-lollies
  [canvas {:keys [palette stroke]} bands w ys scale-y]
  (let [zero (scale-y 0)]
    (doseq [[id y] (map-indexed vector ys)
            :let [{:keys [start end point]} (bands id)
                  x (* w point)
                  size (* w (- end start))
                  col (nth palette id)
                  yy (max 0 (- (scale-y y) (/ size 2)))]]
      (-> canvas
          (set-color col)
          (set-stroke-custom stroke)
          (line x zero x yy)
          (ellipse x yy size size))))
  canvas)

(def ^:private type-fn {:bars draw-rectangles
                        :lollipops draw-lollies})

(defmethod render-graph :histogram [_ data {:keys [color label padding-in padding-out type] :or {type :bars} :as conf}
                                    {:keys [w h x y] :as chart-data}]
  (let [scale-x (partial (:scale x) 0 w)
        scale-y (partial (:scale y) 0 h)
        cnt (count (:samples data))
        bands (s/bands {:padding-out padding-out :padding-in padding-in} cnt)
        draw-fn (type-fn type)]
    (do-graph chart-data false
      (doseq [[x ys] (:bins data)
              :let [xx (scale-x x)
                    xx2 (scale-x (+ x (:step data)))
                    diff (- xx2 xx)]]
        (-> c
            (push-matrix)
            (translate (scale-x x) 0)
            (draw-fn conf bands diff ys scale-y)
            (pop-matrix))))))

