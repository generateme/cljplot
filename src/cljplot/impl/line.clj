(ns cljplot.impl.line
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [clojure2d.color :as c]
            [cljplot.common :refer :all]
            [fastmath.interpolation :as in]
            [fastmath.stats :as stats]
            [fastmath.random :as r]))

(defmethod render-graph :line [_ data {:keys [color stroke interpolation smooth? area? point] :as conf}
                               {:keys [w h x y] :as chart-data}]
  (let [raw-scale-x (:scale x)
        scale-x (partial (:scale x) 0 w)
        scale-y (partial (:scale y) 0 h)
        data (if (fn? interpolation) 
               (let [sampled-values (m/sample (:inverse raw-scale-x) 0.0 1.0 (/ w 2) false)
                     i (wrap-interpolator-for-dt interpolation (:domain x) (map first data) (map second data))]
                 (map #(vector % (i %)) sampled-values))
               data)
        pfn (if (and smooth? (not area?)) path-bezier path)
        lcolor (if area? (c/darken color) color)
        p (map (juxt (comp scale-x first) (comp scale-y second)) data)]
    (do-graph chart-data (#{\o \O} (:type point))
      (when area?
        (set-color c color)
        (path c (conj (vec (conj p [0.0 0.0])) [w 0.0]) true false))
      (-> c
         (set-color lcolor)
         (set-stroke-custom stroke)
         (pfn p))
      (when-let [point-type (:type point)]
        (let [size-fn (:size point)]
          (doseq [[x y :as data] data
                  :let [size (size-fn data conf)]]
            (draw-shape c (scale-x x) (scale-y y) point-type (or (:color point) color) nil size)))))))

;;

(defmethod render-graph :area [_ data conf chart-data]
  (render-graph :line data (assoc conf :area? true) chart-data))

;;

(defmethod prepare-data :function [_ function {:keys [samples domain]}]
  (let [[mx my] domain]
    (m/sample function mx my (or samples 100) true)))

(defmethod data-extent :function [_ sampled {:keys [domain]}]
  {:x [:numerical domain] :y (extent (map second sampled))})

(defmethod render-graph :function [_ sampled conf {:keys [w] :as chart-data}]
  (render-graph :line sampled conf chart-data))

;;

(defmethod prepare-data :density [_ data {:keys [kernel-bandwidth margins] :as conf}]
  (let [dens-data (extract-first data)
        f (if kernel-bandwidth
            (stats/kernel-density dens-data kernel-bandwidth)
            (stats/kernel-density dens-data))
        with-domain (assoc conf :domain (extend-domain-numerical (take 2 (stats/extent dens-data)) (or (:x margins) [0 0])))]
    [with-domain (prepare-data :function f with-domain)]))

(defmethod data-extent :density [_ [with-domain data] _] (data-extent :function data with-domain))
(defmethod render-graph :density [_ [with-domain data] _ graph-conf] (render-graph :function data with-domain graph-conf))

;;

(defmethod prepare-data :cdf [_ data conf]
  (let [data (extract-first data)
        domain (take 2 (stats/extent data))
        f (partial r/cdf (r/distribution :enumerated-real {:data data}))
        with-domain (assoc conf :domain domain)]
    [with-domain (prepare-data :function f with-domain)]))

(defmethod data-extent :cdf [_ [with-domain data] _] (data-extent :function data with-domain))
(defmethod render-graph :cdf [_ [with-domain data] _ graph-conf] (render-graph :function data with-domain graph-conf))

;;

(defn- sarea-map
  [f vs]
  (mapv (fn [[x ys]]
          (let [l (last ys)]
            [x (mapv (partial f l) ys)])) vs))

(defn- sarea-normalized [vs] (sarea-map #(m/norm %2 0 %1) vs))
(defn- sarea-stream [vs] (sarea-map #(let [hl (/ %1 2)] (m/norm %2 0 %1 (- hl) hl)) vs))

(defmethod prepare-data :sarea [_ data {:keys [interpolation method]}]
  (let [xs (->> (vals data)
              (mapcat (partial map first))
              (distinct)
              (sort))
        ks (mapv first data)
        domain [(first xs) (last xs)]
        interp (or interpolation in/linear-smile)
        interpolators (apply juxt
                             (constantly 0)
                             (map (map-kv #(wrap-interpolator-for-dt interp domain (map first %) (map second %)) data) ks))
        vs (mapv #(vector % (reductions + (interpolators %))) xs)
        maxv (reduce max (map (comp last second) vs))]
    (case method
      :normalized [domain [0.0 1.0] (sarea-normalized vs)]
      :stream [domain [(- (/ maxv 2)) (/ maxv 2)] (sarea-stream vs)]
      [domain [0 maxv] vs])))

(defmethod data-extent :sarea [_ [x y _] _]
  {:x [(if (date-time? (first x)) :temporal :numerical) x]
   :y [:numerical y]})

(defmethod render-graph :sarea [_ data {:keys [palette]} {:keys [w h] :as chart-data}]
  (let [a 1]
    (do-graph chart-data false)))

;;

