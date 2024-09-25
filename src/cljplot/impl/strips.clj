(ns cljplot.impl.strips
  (:require [cljplot.common :as common]
            [cljplot.config :as cfg]
            [fastmath.random :as r]
            [clojure2d.core :as c2d]
            [fastmath.stats :as stats]
            [fastmath.kernel :as k]
            [cljplot.scale :as s]
            [clojure2d.color :as c]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- pack-into-seq [data]
  (if (sequential? (first data)) data (map vector data)))


(defmethod common/prepare-data :rug [_ data _] (pack-into-seq data))
(defmethod common/render-graph :rug [_ data {:keys [color size cap distort ^double scale] :as conf} {:keys [w ^int h x] :as chart-data}]
  (let [scale-x (:scale x)]
    (common/do-graph chart-data false
                     (doseq [[^double v :as all] data
                             :let [x (scale-x 0 w (+ ^double (r/grand distort) v))]]
                       (-> c
                           (c2d/set-color (color all conf))
                           (c2d/set-stroke (size all conf) cap) 
                           (c2d/line x 2 x (* scale (- h 3))))))))

;;


(defmethod common/prepare-data :strip [_ data _] (pack-into-seq data))
(defmethod common/render-graph :strip [_ data {:keys [color size shape ^double distort ^double scale] :as conf} {:keys [w ^int h x] :as chart-data}]
  (let [scale-x (:scale x)]
    (common/do-graph chart-data (#{\o \O} shape)
                     (doseq [[^double v :as all] data
                             :let [x (scale-x 0 w (+ ^double (r/grand distort) v))
                                   y (+ ^double (r/grand distort) (* 0.5 h scale))]]
                       (common/draw-shape c x y shape (color all conf) nil (size all conf))))))

;;

(def ^:private extents-map
  {:min-max stats/extent
   :iqr stats/percentile-extent
   :adjacent stats/adjacent-values
   :stddev stats/stddev-extent
   :mad stats/mad-extent
   :sem stats/sem-extent
   :ci stats/ci
   :bci stats/bootstrap-ci})

(defmethod common/prepare-data :extent-stat [_ data {ext :extent-type}]
  (let [d (common/extract-first data)]
    (cond
      (and (keyword? ext) (contains? extents-map ext)) ((extents-map ext) d)
      (fn? ext) (ext d)
      :else (stats/extent d))))

(defmethod common/data-extent :extent-stat [_ [x y] _]  {:x [:numerical [x y]]})

(defmethod common/render-graph :extent-stat [_ [min-val max-val stat-val] {:keys [color size shape stroke]}
                                             {:keys [w ^int h x] :as chart-data}]
  (let [scale-x (:scale x)
        h2 (* 0.5 h)]
    (common/do-graph chart-data (#{\o \O} shape)
                     (-> (c2d/set-color c color)
                         (c2d/set-stroke-custom stroke)
                         (c2d/line (scale-x 0 w min-val) h2 (scale-x 0 w max-val) h2))
                     (when shape (common/draw-shape c (scale-x 0 w stat-val) h2 shape (c/darken color) nil size)))))

;;

(defmethod common/prepare-data :box [_ data _] (stats/stats-map (common/extract-first data)))

(defmethod common/data-extent :box [_ data _]
  {:x [:numerical [(:Min data) (:Max data)]]})

(defmethod common/render-graph :box [_ data {:keys [color ^double size shape outliers?] :as conf} {:keys [^int w ^int h x] :as chart-data}]
  (let [color (color nil conf)
        dcolor (c/darken color)
        line-cl (if (> 127.5 ^double (c/luma color)) :black :white)
        scale-x (partial (:scale x) 0 w)
        [median ^double q1 ^double q3 lav uav] (map scale-x (map data [:Median :Q1 :Q3 :LAV :UAV :LIF :UIF]))
        h2 (* 0.5 h)
        [^double hl ^double hh ^double hlm ^double hhm] (map #(+ h2 (* ^double % h2)) [-0.5 0.5 -0.25 0.25])]
    (common/do-graph chart-data (and outliers? (#{\o \O} shape))
                     (-> c
                         (c2d/set-color dcolor)
                         (c2d/set-stroke size)
                         (c2d/line lav h2 uav h2)
                         (c2d/line lav hlm lav hhm)
                         (c2d/line uav hlm uav hhm)
                         (c2d/set-color dcolor 200)
                         (c2d/filled-with-stroke color dcolor c2d/rect (dec q1) hl (inc (- q3 q1)) (- hh hl))
                         (c2d/set-color line-cl 200)
                         (c2d/line median (inc hl) median (dec hh)))
                     (when outliers?
                       (doseq [o (map scale-x (:Outliers data))]
                         (common/draw-shape c o (+ h2 (r/grand)) shape (c/color color 200) nil (max 3.0 (* 0.7 size (- hhm hlm)))))))))

;;

(defmethod common/prepare-data :violin [_ data {:keys [kernel-bandwidth samples margins kernel-type]
                                                :or {kernel-type :gaussian}}]
  (let [dens-data (common/extract-first data)
        stats (stats/stats-map dens-data)
        [mn mx] (common/extend-domain-numerical [(:Min stats) (:Max stats)] (or (:x margins) [0.0 0.0]))
        density (m/sample (if kernel-bandwidth
                            (k/kernel-density kernel-type dens-data kernel-bandwidth)
                            (k/kernel-density kernel-type dens-data)) mn mx (or samples 100) true)]
    [density stats]))

(defmethod common/data-extent :violin [_ [density stats] _]
  (let [[_ ^double ymx] (stats/extent (map second density))]
    {:x [:numerical [(:Min stats) (:Max stats)]]
     :y [:numerical [(- ymx) ymx]]}))

(defmethod common/render-graph :violin [_ [density stats] {:keys [color color-bar ^double size-bar size ^double scale] :as conf}
                                        {:keys [w h x y] :as chart-data}]
  (let [color (color nil conf)
        scale-x (partial (:scale x) 0 w)
        scale-y (partial (:scale y) 0 h)
        p1 (map (fn [[^double x ^double y]] [(scale-x x) (scale-y (* y scale))]) density)
        p2 (map (fn [[^double x ^double y]] [(scale-x x) (scale-y (- (* y scale)))]) density)
        ^double zero (scale-y 0.0)
        [median ^double q1 ^double q3 lav uav] (map scale-x (map stats [:Median :Q1 :Q3 :LAV :UAV]))]
    (common/do-graph chart-data false
                     (-> (c2d/set-stroke c size)
                         (c2d/filled-with-stroke color (c/darken color) c2d/path (concat p1 (reverse p2)) true)
                         (c2d/set-color color-bar)
                         (c2d/line lav zero uav zero)
                         (c2d/rect q1 (- zero size-bar) (- q3 q1) (+ size-bar size-bar))
                         (c2d/set-color (c/brighten color))
                         (c2d/crect median zero 3 3)))))

;;

(defmethod common/prepare-data :density-strip [_ data {:keys [kernel-bandwidth samples margins kernel-type]
                                                       :or {kernel-type :gaussian}}]
  (let [dens-data (common/extract-first data)
        all (stats/extent dens-data)
        [mn mx] (common/extend-domain-numerical all (or (:x margins) [0.0 0.0]))
        density (m/sample (if kernel-bandwidth
                            (k/kernel-density kernel-type dens-data kernel-bandwidth)
                            (k/kernel-density kernel-type dens-data)) mn mx (or samples 100) true)]
    [density all]))

(defmethod common/data-extent :density-strip [_ [density [xmn xmx]] _]
  (let [[_ ^double ymx] (stats/extent (map second density))]
    {:x [:numerical [xmn xmx]]
     :y [:numerical [(- ymx) ymx]]}))

(defmethod common/render-graph :density-strip [_ [density] {:keys [area? color size ^double scale] :as conf}
                                               {:keys [w h x y] :as chart-data}]
  (let [scale-x (partial (:scale x) 0 w)
        scale-y (partial (:scale y) 0 h)
        zero (scale-y 0.0)
        p (map (fn [[^double x ^double y]] [(scale-x x) (scale-y (* y scale))]) density)
        p (conj (vec (conj p [(ffirst p) zero])) [(first (last p)) zero])
        col (color density conf)]
    (common/do-graph chart-data false
                     (c2d/set-stroke c size)
                     (if area?
                       (c2d/filled-with-stroke c col (c/darken col) c2d/path p true)
                       (do
                         (c2d/set-color c col)
                         (c2d/path c p false true))))))


;;

#_(defn- draw-rectangles
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

#_(defn- draw-lollies
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

(defmethod common/prepare-data :bar [_ data _]
  (if (sequential? data) data [data]))

(defmethod common/data-extent :bar [_ data _]
  {:x (common/find-min-max (conj (map #(vector :numerical [% %]) data) [:numerical [0 0]]))
   :y [:numerical [0.0 1.0]]})

(defmethod common/render-graph :bar [_ data {:keys [palette color stroke stroke? padding-in padding-out] :as conf}
                                     {:keys [w ^int h x] :as chart-data}]
  (let [cnt (count data)
        bands (s/bands {:padding-out padding-out :padding-in padding-in} cnt)
        scale-x (partial (:scale x) 0 w)
        ^double zero (scale-x 0)
        pal (if (seq palette) (cycle palette) (repeat color))
        col? (and color (= cnt 1))]
    (common/do-graph chart-data false
                     (c2d/set-stroke-custom c stroke)
                     (doseq [[^long id ^double v] (map-indexed vector data)
                             :let [{:keys [^double start ^double end]} (bands (- cnt id 1))
                                   st (* start h)
                                   hh (* (- end start) h) 
                                   col (if col? (color v conf) (nth pal id))
                                   ^double sv (scale-x v)
                                   [x w] (if (neg? v) [sv (- zero sv)] [zero (- sv zero)])]]
                       (if stroke?
                         (-> c
                             (c2d/set-stroke-custom stroke)
                             (c2d/filled-with-stroke col (c/darken col) c2d/rect x st w hh))
                         (-> c
                             (c2d/set-color col) 
                             (c2d/rect x st w hh)))))))

(defmethod common/prepare-data :lollipop [_ data conf] (common/prepare-data :bar data conf))
(defmethod common/data-extent :lollipop [_ data conf] (common/data-extent :bar data conf))
(defmethod common/render-graph :lollipop [_ data {:keys [palette color stroke padding-in padding-out ^double size]
                                                  :or {size 0.3} :as conf}
                                          {:keys [^int w ^int h x] :as chart-data}]
  (let [cnt (count data)
        bands (s/bands {:padding-out padding-out :padding-in padding-in} cnt)
        scale-x (partial (:scale x) 0 w)
        ^double zero (scale-x 0)
        pal (if (seq palette) (cycle palette) (repeat color))
        col? (and color (= cnt 1))]
    (common/do-graph chart-data false
                     (doseq [[^long id ^double v] (map-indexed vector data)
                             :let [{:keys [^double start ^double end ^double point]} (bands (- cnt id 1))
                                   hh (* point h)
                                   col (if col? (color v conf) (nth pal id))
                                   ^double sv (scale-x v)
                                   size (* h size (- end start))
                                   [x w] (if (neg? v) [sv (- zero sv)] [zero (- sv zero)])]]
                       (-> (c2d/set-color c col)
                           (c2d/set-stroke-custom stroke)
                           (c2d/line x hh w hh)
                           (c2d/ellipse w hh size size))))))



(defmethod common/data-extent :rbar [_ data _]
  {:x [:numerical data]
   :y [:numerical [0.0 1.0]]})

(defmethod common/render-graph :rbar [_ data {:keys [color stroke stroke? padding] :as conf}
                                      {:keys [w ^int h x] :as chart-data}]
  (let [{:keys [^double start ^double end]} ((s/bands {:padding-out padding} 1) 0)
        st (* start h)
        hh (* (- end start) h)
        scale-x (partial (:scale x) 0 w)]
    (common/do-graph chart-data false
                     (c2d/set-stroke-custom c stroke)
                     (let [[x y] data
                           col (color data conf)
                           ^double sv (scale-x x)
                           ww (- ^double (scale-x y) sv)]
                       (if stroke?
                         (-> c
                             (c2d/set-stroke-custom stroke)
                             (c2d/filled-with-stroke col (c/darken col) c2d/rect sv st ww hh))
                         (-> c
                             (c2d/set-color col) 
                             (c2d/rect sv st ww hh)))))))


(defmethod common/prepare-data :sbar [_ data {:keys [method]}]
  (let [data (if (sequential? data) data [data])]
    (case method
      :layered {:sum (reduce m/max data)
                :data data}
      :normalized {:sum 1.0
                   :data (mapv #(/ ^double % ^double (reduce m/+ data)) data)} 
      {:sum (reduce m/+ data)
       :data data})))

(defmethod common/data-extent :sbar [_ {:keys [sum]} _]
  {:x [:numerical [0.0 sum]]
   :y [:numerical [0.0 1.0]]})

(defmethod common/render-graph :sbar [_ {:keys [data]} {:keys [method palette stroke stroke? padding]}
                                      {:keys [w ^int h x] :as chart-data}]
  (let [{:keys [^double start ^double end]} ((s/bands {:padding-out padding} 1) 0)
        scale-x (partial (:scale x) 0 w)
        zero (scale-x 0)
        pal (cycle palette)
        st (* start h)
        hh (* (- end start) h)]
    (common/do-graph chart-data false
                     (c2d/set-stroke-custom c stroke)
                     (c2d/translate c zero 0)
                     (doseq [[id v] (map-indexed vector data)
                             :let [col (nth pal id)
                                   ^double sv (scale-x v)]]
                       (if stroke?
                         (-> c
                             (c2d/set-stroke-custom stroke)
                             (c2d/filled-with-stroke col (c/darken col) c2d/rect 0 st (dec sv) hh))
                         (-> c
                             (c2d/set-color col) 
                             (c2d/rect 0 st sv hh)))
                       (when-not (= method :layered) (c2d/translate c sv 0))))))

;;;;;;;;;;;;;;;;;;;;

(defmethod common/prepare-data :stack [_ [h? t data c] _]
  (let [c (cfg/merge-configuration t (or c {}))]
    [h? t (map-indexed (fn [iter [k v]]
                         (let [cc (assoc c :id k :series-id iter :chart-type t)
                               dd (common/prepare-data t v cc)
                               ex (common/extend-domains (common/data-extent t dd cc) (:margins cc))]
                           [k dd (assoc cc :extents ex)])) data) c]))

(defmethod common/data-extent :stack [_ [h? _t data _c] {:keys [padding-out padding-in]}]
  (let [extents (map (comp :extents last) data)
        bands [:categorical (map first data) {:padding-out padding-out :padding-in padding-in}]
        x (common/find-min-max (map :x extents))
        y (common/find-min-max (map :y extents))]
    (assoc (if h? {:x x :y bands} {:x bands :y x}) :inner y)))

(defmethod common/render-graph :stack [_ [h? t data _c] conf {:keys [w h x y] :as chart-data}]
  (let [inner (s/scale-map [:linear] {:domain (-> conf :extent :inner second)})
        [lx ly sy width height oc og] (if h? [x inner (:scale y) w h :bottom :top] [y inner (:scale x) h w :left :bottom])
        sizes (common/bands->positions-size sy height)
        chart-data-inner {:x lx :y ly :w width :orientation oc}
        charts (for [[k d cc] data]
                 [k (common/render-graph t d cc
                                         (assoc chart-data-inner :h (second (sizes k))))])]
    (common/do-graph (assoc chart-data :orientation og) false
      (doseq [[k v] (reverse charts)
              :let [[^double p] (sizes k)]]
        (-> c
            (c2d/push-matrix)
            (c2d/translate (:anchor v)))
        (if h? (c2d/image c (:canvas v) 0 p) (c2d/image c (:canvas v) p -2))
        (c2d/pop-matrix c)))))

;; delegates

(defmethod common/prepare-data :stack-horizontal [_ [t data c] conf] (common/prepare-data :stack [true t data c] conf))
(defmethod common/data-extent :stack-horizontal [_ d conf] (common/data-extent :stack d conf))
(defmethod common/render-graph :stack-horizontal [_ d conf chart-data] (common/render-graph :stack d conf chart-data))

(defmethod common/prepare-data :stack-vertical [_ [t data c] conf] (common/prepare-data :stack [false t data c] conf))
(defmethod common/data-extent :stack-vertical [_ d conf] (common/data-extent :stack d conf))
(defmethod common/render-graph :stack-vertical [_ d conf chart-data] (common/render-graph :stack d conf chart-data))

(m/unuse-primitive-operators)
