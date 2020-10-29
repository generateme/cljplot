(ns cljplot.impl.line
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [clojure2d.color :as c]
            [cljplot.common :refer :all]
            [fastmath.interpolation :as in]
            [fastmath.stats :as stats]
            [fastmath.kernel :as k]
            [fastmath.random :as r]
            [fastmath.protocols :as pr]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)


(defn- process-interpolation
  [data interpolation x w]
  (if (fn? interpolation) 
    (let [sampled-values (m/sample (:inverse (:scale x)) 0.0 1.0 (/ ^int w 2) false)
          i (wrap-interpolator-for-dt interpolation (:domain x) (map first data) (map second data))]
      (map #(vector % (i %)) sampled-values))
    data))

;; TODO: move interpolation to the prepare-data phase (like in sarea)

(defn- split-at-invalid-double
  [xs]
  (loop [s xs
         buff []]
    (if-not (seq s)
      buff
      (let [[v inv] (split-with (comp m/valid-double? second) s)]
        (recur (drop-while (comp m/invalid-double? second) inv)
               (if (seq v) (conj buff v) buff))))))

(defmethod render-graph :line [_ data {:keys [color stroke interpolation smooth? area? point] :as conf}
                               {:keys [w h x y] :as chart-data}]
  (let [scale-x (partial (:scale x) 0 w)
        scale-y (partial (:scale y) 0 h) 
        data (map #(process-interpolation % interpolation x w) (split-at-invalid-double data))
        pfn (if (and smooth? (not area?)) path-bezier path)
        lcolor (if area? (c/darken color) color)
        ps (map #(map (juxt (comp scale-x first) (comp scale-y second)) %) data)]
    (do-graph (assoc chart-data :oversize 0) true
              #_(#{\o \O} (:type point))
              (doseq [p ps]
                (when area?
                  (set-color c color)
                  (pfn c (conj (vec (conj p [0.0 0.0])) [w 0.0]) true false))
                (-> c
                    (set-color lcolor)
                    (set-stroke-custom stroke)
                    (pfn p))

                (when-let [point-type (:type point)]
                  (let [size-fn (:size point)
                        stroke (:stroke point)]
                    (doseq [[x y :as dta] p
                            :let [size (size-fn dta conf)]]
                      (draw-shape c x y point-type (or (:color point) color) stroke size))))))))

;;

(defmethod render-graph :area [_ data conf chart-data]
  (render-graph :line data (assoc conf :area? true) chart-data))

;;

(defmethod data-extent :ci [_ [top bottom] _] (common-extent (concat top bottom)))
(defmethod render-graph :ci [_ [top bottom] {:keys [color stroke interpolation smooth?] :as conf}
                             {:keys [w h x y] :as chart-data}]
  (let [scale-x (partial (:scale x) 0 w)
        scale-y (partial (:scale y) 0 h)
        data-top (process-interpolation top interpolation x w)
        data-bottom (process-interpolation bottom interpolation x w)
        p-top (map (juxt (comp scale-x first) (comp scale-y second)) data-top)
        p-bottom (map (juxt (comp scale-x first) (comp scale-y second)) data-bottom)]
    (do-graph chart-data (#{\o \O} (:type point))
              (-> c
                  (set-color color)
                  (path (concat p-top (reverse p-bottom)) true false))
              (when stroke
                (-> c
                    (set-color (c/darken color))
                    (set-stroke-custom stroke)
                    (path p-top)
                    (path p-bottom))))))
;;

(defmethod prepare-data :function [_ function {:keys [samples domain]}]
  (let [[mx my] domain]
    (m/sample function mx my (or samples 100) true)))

(defmethod data-extent :function [_ sampled {:keys [domain]}]
  {:x [:numerical domain] :y (extent (map second sampled))})

(defmethod render-graph :function [_ sampled conf {:keys [w] :as chart-data}]
  (render-graph :line sampled conf chart-data))

;;

(defmethod prepare-data :density [_ data {:keys [kernel-bandwidth kernel-type margins]
                                          :or {kernel-type :smile}
                                          :as conf}]
  (let [dens-data (extract-first data)
        f (if kernel-bandwidth
            (k/kernel-density kernel-type dens-data kernel-bandwidth)
            (k/kernel-density kernel-type dens-data))
        with-domain (assoc conf :domain (extend-domain-numerical (take 2 (stats/extent dens-data)) (or (:x margins) [0 0])))]
    [with-domain (prepare-data :function f with-domain)]))

(defmethod data-extent :density [_ [with-domain data] _] (data-extent :function data with-domain))
(defmethod render-graph :density [_ [with-domain data] _ graph-conf] (render-graph :function data with-domain graph-conf))

;;

(defmethod prepare-data :cdf [_ data conf]
  (if (satisfies? pr/DistributionProto data)
    [conf (prepare-data :function (partial r/cdf data) conf)]
    (let [data (extract-first data)
          domain (take 2 (stats/extent data))
          f (partial r/cdf (r/distribution :enumerated-real {:data (sort data)}))
          with-domain (assoc conf :domain domain)]
      [with-domain (prepare-data :function f with-domain)])))

(defmethod data-extent :cdf [_ [with-domain data] _] (data-extent :function data with-domain))
(defmethod render-graph :cdf [_ [with-domain data] _ graph-conf] (render-graph :function data with-domain graph-conf))

;;

(defn- sarea-map
  [f vs]
  (mapv (fn [[x ys]]
          (let [l (last ys)]
            [x (mapv (partial f l) ys)])) vs))

(defn- sarea-normalized [vs] (sarea-map #(m/norm %2 0 %1) vs))
(defn- sarea-stream [vs] (sarea-map #(let [hl (/ ^int %1 2)] (m/norm %2 0 %1 (- hl) hl)) vs))

(defmethod prepare-data :sarea [_ data {:keys [interpolation method]}]
  (let [xs (->> (map second data)
                (mapcat (partial map first))
                (distinct)
                (sort))
        ks (mapv first data)
        domain [(first xs) (last xs)]
        interp (or interpolation in/linear-smile)
        data-as-map (if (map? data) data (into {} data))
        interpolators (apply juxt
                             (constantly 0)
                             (map (map-kv #(wrap-interpolator-for-dt interp domain (map first %) (map second %)) data-as-map) ks))
        vs (mapv #(vector % (reductions fast+ (interpolators %))) xs)
        ^double maxv (reduce fast-max (mapv (comp last second) vs))]
    (case method
      :normalized [domain [0.0 1.0] ks (sarea-normalized vs)]
      :stream [domain [(- (/ maxv 2)) (/ maxv 2)] ks (sarea-stream vs)]
      [domain [0 maxv] ks vs])))

(defmethod data-extent :sarea [_ [x y] _]
  {:x [(if (date-time? (first x)) :temporal :numerical) x]
   :y [:numerical y]})

(defmethod render-graph :sarea [_ [_ _ ks vs] {:keys [palette]} {:keys [w h x y] :as chart-data}]
  (let [scale-x (partial (:scale x) 0 w)
        scale-y (partial (:scale y) 0 h)
        xs (mapv scale-x (map first vs)) 
        ys (mapv second vs)
        ys (map #(mapv (fn [x y] [x (scale-y (nth y %))]) xs ys) (range (inc (count ks))))
        ps (map (fn [[p1 p2]] (concat p1 (reverse p2))) (partition 2 1 ys))]
    (do-graph chart-data false
      (set-color c :black)
      (doseq [[col p] (map vector palette ps)]
        (set-color c col)
        (path c p true false)))))

;; lines

(defmethod data-extent :abline [_ _ _] nil)
(defmethod render-graph :abline [_ [a b x1 x2] {:keys [color] :as conf} {:keys [w h x y extent] :as chart-data}]
  (let [scale-x (partial (:scale x) 0 w)
        scale-y (partial (:scale y) 0 h)
        ^double a (or a 1.0)
        ^double b (or b 0.0)
        [mnx mxx] (:x extent)
        ^double x1 (or x1 mnx)
        ^double x2 (or x2 mxx)
        y1 (+ b (* a x1))
        y2 (+ b (* a x2))]
    (do-graph (assoc chart-data :oversize 0) false
      (-> (set-color c color)
          (set-stroke-custom conf)
          (line (scale-x x1) (scale-y y1) (scale-x x2) (scale-y y2))))))

(defmethod data-extent :vline [_ _ _] nil)
(defmethod render-graph :vline [_ xx {:keys [color] :as conf} {:keys [w ^int h x] :as chart-data}]
  (let [xx ((:scale x) 0 w (or xx 0.0))]
    (do-graph (assoc chart-data :oversize 0) false
      (-> (set-color c color)
          (set-stroke-custom conf)
          (line xx 0 xx (dec h))))))

(defmethod data-extent :hline [_ _ _] nil)
(defmethod render-graph :hline [_ yy {:keys [color] :as conf} {:keys [^int w h y] :as chart-data}]
  (let [yy ((:scale y) 0 h (or yy 0.0))]
    (do-graph (assoc chart-data :oversize 0) false
      (-> (set-color c color)
          (set-stroke-custom conf)
          (line 0 yy (dec w) yy)))))
