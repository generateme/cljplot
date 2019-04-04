(ns cljplot.impl.time-series
  (:require [clojure2d.core :refer :all]
            [cljplot.common :refer :all]
            [fastmath.stats :as stats]
            [fastmath.core :as m]
            [cljplot.config :as cfg]
            [fastmath.random :as rnd]
            [fastmath.interpolation :as in]))

(defmethod prepare-data :lag [_ data {:keys [lag]}]
  (map vector data (drop (or lag 1) data)))

(defmethod render-graph :lag [_ data conf chart-data] (render-graph :scatter data conf chart-data))

(defonce ^:private gaussian (rnd/distribution :normal))

(defmethod prepare-data :acf [_ data {:keys [lags]}]
  (let [vdata (vec data)
        lags (or lags (min (dec (count vdata)) (m/ceil (* 10.0 (m/log10 (count vdata))))))]
    (let [acf (map (fn [lag]
                     (let [v2 (subvec vdata lag)
                           v1 (subvec vdata 0 (count v2))]
                       [lag (stats/correlation v1 v2)])) (range (inc lags)))
          rsqrt (/ 1.0 (m/sqrt (count vdata)))
          ci (* rsqrt (rnd/icdf gaussian (* 0.5 (inc 0.95))))
          psums (map #(* ci (m/sqrt (dec (* 2.0 %))))
                     (reductions (fn [acc s] (+ acc (* s s))) (map second acf)))]
      [acf rsqrt ci psums])))

(defmethod data-extent :acf [_ data _]
  (let [e (common-extent (first data))
        [_ [mn mx]] (:y e)]
    (assoc-in e [:y 1] [(min 0.0 mn) mx])))

;; pars: ci
(defmethod render-graph :acf [_ [data rsqrt ci0 cis] conf {:keys [w h x y] :as chart-data}]
  (println (take 5 cis))
  (let [scale-x (partial (:scale x) 0 w)
        scale-y (partial (:scale y) 0 h)
        zero (scale-y 0.0)
        w- (dec w)
        ci- (scale-y (- ci0))
        ci (scale-y ci0)
        p1 (map-indexed #(vector (scale-x %1) (scale-y %2)) cis)
        p2 (reverse (map-indexed #(vector (scale-x %1) (scale-y (- %2))) cis))]
    (do-graph (assoc chart-data :oversize 0) false
      (-> c
          (set-color :grey 80)
          (path (concat p2 p1) true false)
          (rect 0 ci- w (- ci ci-))
          (line 0 ci w- ci)
          (line 0 ci- w- ci-)
          (set-color :black)
          (line 0 zero w- zero)) 
      (doseq [[x y] data
              :let [xx (scale-x x)
                    yy (scale-y y)]]
        (line c xx zero xx yy)
        (ellipse c xx yy 3 3)))))

