(ns cljplot.common
  (:require [clojure2d.core :refer :all]
            [clojure2d.color :as c]
            [fastmath.stats :as stats]
            [fastmath.grid :as grid]
            [fastmath.vector :as v]
            [fastmath.core :as m]
            [clojure.data.csv :as csv]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [java-time :as dt]
            [cljplot.scale.time :as t]
            [cljplot.scale :as s]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn fast+ {:inline (fn [^double x ^double y] `(+ ~x ~y)) :inline-arities #{2}}
  ^double [^double a ^double b] (+ a b))

(defn fast- {:inline (fn [^double x ^double y] `(- ~x ~y)) :inline-arities #{2}}
  ^double [^double a ^double b] (- a b))

(defn fast* {:inline (fn [^double x ^double y] `(* ~x ~y)) :inline-arities #{2}}
  ^double [^double a ^double b] (* a b))

(defn fast-max {:inline (fn [^double x ^double y] `(+ ~x ~y)) :inline-arities #{2}}
  ^double [^double a ^double b] (max a b))

(defn fast-min {:inline (fn [^double x ^double y] `(+ ~x ~y)) :inline-arities #{2}}
  ^double [^double a ^double b] (min a b))

(defn graph-canvas
  "Create canvas to draw a chart on"
  ([graph-conf] (graph-canvas graph-conf false))
  ([{:keys [w h orientation rendering-hint ^double oversize] :or {orientation :top oversize 100}} highest?]
   (let [[^int cw ^int ch] (if (#{:left :right} orientation) [h w] [w h])
         canvas-shift (/ oversize 2)
         canvas-shift- (- canvas-shift)
         c (canvas (+ oversize cw) (+ oversize ch) (if rendering-hint
                                                     rendering-hint
                                                     (if highest? :highest :high)))]
     {:canvas c
      :anchor [canvas-shift- canvas-shift-]
      :shift [canvas-shift canvas-shift]
      :w w
      :h h})))

(defn canvas-orientation
  "Convert layout orientation to canvas orientations"
  [orient]
  (get {:bottom :top-left- :top :bottom-left+ :left :bottom-right+ :right :bottom-left-} orient :bottom-left+))

(defmacro do-graph
  "Wrap canvas creation and orientation."
  {:style/indent 2}
  ([graph-conf highest-render? & body]
   (let [c (symbol "c")]
     `(let [canv# (graph-canvas ~graph-conf ~highest-render?)
            orient# (canvas-orientation (:orientation ~graph-conf))]
        (with-oriented-canvas orient# [~c (:canvas canv#)]
          (translate ~c (:shift canv#))
          ~@body)
        canv#))))

;;

(defn date-time?
  [t]
  (instance? java.time.temporal.Temporal t))

(defn wrap-interpolator-for-dt
  "Wrap interpolator to work with date/time values."
  [interpolator domain x y]
  (if (date-time? (first x))
    (let [scale (t/time-scale domain)
          interp (interpolator (mapv scale x) y)]
      (fn [v]
        (interp (scale v))))
    (interpolator x y)))

;;

(defn deep-merge
  "Merge maps on every level."
  [a b] (if (and (map? a) (map? b)) (merge-with deep-merge a b) b))

(defn map-kv [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))

(defn find-min-max
  "For given list of vectors find maximum extent."
  [s]
  (let [s (filter seq s)]
    (when (seq s)
      (assert (-> (map first s)
                  (set)
                  (count)
                  (dec)
                  (zero?)) "Types of series should have the same type: continuous or categorical")
      (let [[t _ sconf] (first s)
            res [t (if (#{:numerical :temporal} t)
                     (let [[fmin fmax] (if (= t :temporal) [dt/min dt/max] [fast-min fast-max])]
                       (reduce (fn [[ca cb] [a b]] [(fmin ca a) (fmax cb b)]) (map second s)))
                     (distinct (mapcat identity (map second s))))]]
        (if sconf (conj res sconf) res)))))

(defn extent
  [data]
  (let [data (remove #(or (nil? %)
                          (and (number? %)
                               (m/invalid-double? %))) data)
        v (first data)]
    (cond
      (date-time? v) [:temporal [(reduce dt/min data) (reduce dt/max data)]]
      (number? v) [:numerical (take 2 (stats/extent data))])))

;;

(defn bands->positions-size
  [bands ^double size]
  (into {} (mapv (fn [id]
                   (let [{:keys [^double start ^double end]} (bands id)
                         ss (m/round (- (* size end) (* size start)))
                         st (m/round (* size start))]
                     [id [st ss]])) (:domain bands))))

(defn common-extent
  "Calculate extent for axes"
  ([data] (common-extent data first second #(nth % 2)))
  ([data selector-x selector-y selector-z]
   (if-not (sequential? (first data))
     {:x (extent data)
      :y [:numerical [0.0 1.0]]}
     (let [cnt (count (first data))]
       (cond-> {}
         (pos? cnt) (assoc :x (extent (map selector-x data)))
         (> cnt 1) (assoc :y (extent (map selector-y data)))
         (> cnt 2) (assoc :z (extent (map selector-z data))))))))


(defn extend-domain-numerical
  [[^double start ^double end] [^double l ^double r]]
  (let [diff (- end start)]
    [(- start (* diff l))
     (+ end (* diff r))]))

(defn- extend-domain
  [[t domain :as all] margin]
  (case t
    :numerical [t (extend-domain-numerical domain margin)]
    all))

(defn- check-and-extend
  [extent margin]
  (if (and extent margin)
    (extend-domain extent margin)
    extent))

(defn extend-domains
  [extents margins]
  (-> extents
      (update :x check-and-extend (:x margins))
      (update :y check-and-extend (:y margins))))

(defn extract-first [data]
  (if (sequential? (first data))
    (map first data)
    data))

(defn coerce-format-fn
  "Find formatting function."
  [fmt]
  (cond
    (string? fmt) (partial format fmt)
    (fn? fmt) fmt
    :else str))


;;

(defmulti data-extent (fn [t data config] t))
(defmulti render-graph (fn [t data config chart-data] t))
(defmulti prepare-data (fn [t data config] t))
(defmulti postprocess-data (fn [t data config] t))

(defmethod data-extent :default [_ data _] (common-extent data))
(defmethod prepare-data :default [_ data _] data)
(defmethod postprocess-data :default [_ data _] data)
(defmethod render-graph :default [_ _ _ chart-data] (do-graph chart-data false))

(defmethod data-extent :empty [_ _ _] {})

;;

(defn get-scale
  "Return scale from series"
  [series key]
  (:scale (series key)))

(defn get-width
  [series]
  (:size (:x series)))

(defn get-height
  [series]
  (:size (:x series)))

;;

(defn heatmap-grid
  "Snap normalized coords from vx to grid cell midpoints."
  [vx scale-x scale-y gtype cells]
  (let [iscale-x (:inverse scale-x)
        iscale-y (:inverse scale-y)
        g (grid/grid gtype (/ (double cells)))
        cnts (reduce (fn [m [x y cnt]]
                       (let [cnt (or cnt 1)
                             cell (grid/coords->mid g (v/vec2 (scale-x x) (scale-y y)))]
                         (if (contains? m cell)
                           (update m cell fast+ cnt)
                           (assoc m cell cnt)))) {} vx)]
    (map (fn [[[x y] cnt]]
           (v/vec3 (iscale-x x) (iscale-y y) cnt)) cnts)))

;;

(defn- triangle-shape 
  ""
  [canv x y hsize size angle stroke?]
  (let [size (double size)
        hsize (double hsize)
        size3 (/ size 3.0)
        size6 (+ size3 size3)]
    (-> canv
        (push-matrix)
        (translate x y)
        (rotate (m/radians angle))
        (triangle (- hsize) (- size3)
                  (+ hsize) (- size3)
                  0 (+ size6) stroke?)
        (pop-matrix))))

(defn draw-shape 
  ""
  [canv x y type color stroke size]
  (-> canv
      (set-stroke-custom stroke)
      (set-color color))
  (let [size (double size)
        x (double x)
        y (double y)
        hsize (/ size 2.0)]
    (case type
      \* (-> canv
             (set-font-attributes (* 2 size))
             (text (str type) x (+ (- (font-height canv) (* 0.6666 (font-ascent canv))) y) :center))
      \} (filled-with-stroke canv color (c/darken color) triangle-shape x y hsize size -90)
      \{ (filled-with-stroke canv color (c/darken color) triangle-shape x y hsize size 90)
      \A (filled-with-stroke canv color (c/darken color) triangle-shape x y hsize size 180)
      \V (filled-with-stroke canv color (c/darken color) triangle-shape x y hsize size 0)
      \v (triangle-shape canv x y hsize size 0 true)
      \> (triangle-shape canv x y hsize size -90 true)
      \< (triangle-shape canv x y hsize size 90 true)
      \^ (triangle-shape canv x y hsize size 180 true)
      \x (-> canv
             (line (- x hsize) (+ y hsize) (+ x hsize) (- y hsize))
             (line (- x hsize) (- y hsize) (+ x hsize) (+ y hsize)))
      \/ (line canv (- x hsize) (+ y hsize) (+ x hsize) (- y hsize))
      \\ (line canv (- x hsize) (- y hsize) (+ x hsize) (+ y hsize))
      \+ (-> canv
             (line (- x hsize) y (+ x hsize) y)
             (line x (- y hsize) x (+ y hsize)))
      \- (line canv (- x hsize) y (+ x hsize) y)
      \| (line canv x (- y hsize) x (+ y hsize))
      \s (crect canv x y size size true)
      \S (filled-with-stroke canv color (c/darken color) crect x y size size)
      \o (ellipse canv x y size size true)
      \O (filled-with-stroke canv color (c/darken color) ellipse x y size size)
      \. (point canv x y)
      (let [[^double sx ^double sy] (map #(m/ceil %) (text-bounding-box canv (str type)))]
        (-> canv
            (push-matrix)
            (translate (+ x sx) (- y (/ sy 2)))
            (set-font-attributes size)
            (text (str type) 0 0 :center)
            (pop-matrix))))))

;;

(defn transformed-text
  [c s x y & r]
  (push-matrix c)
  (let [[nx ny] (transform c x y)]
    (reset-matrix c)
    (apply text c s nx ny r)
    (pop-matrix c)))

(def line-dash-styles
  (cycle [[1 1] [2 2] [4 4] [5 1 3] [4 1] [10 2] [14 2 7 2] [14 2 2 7] [2 2 20 2 20 2]]))

;;

(defn render-label
  ""
  [label ^long w]
  (let [ww (int (* 0.95 w))
        sx (/ (- w ww) 2)]
    (with-canvas-> (canvas w 12)
      (set-stroke 0.5)
      (filled-with-stroke (c/color :white 150) (c/color :black 100) rect sx 1 ww 10)
      (set-font-attributes 10)
      (text label (/ w 2) 10 :center))))

;;

(defn label-size
  ([s] (label-size s {}))
  ([s {:keys [font font-size ^double margin] :or {margin 8}}]
   (with-canvas [c (canvas 1 1)]
     (when font (set-font c font))
     (when font-size (set-font-attributes c font-size))
     (let [[x ^double y _ h] (text-bounding-box c s)]
       {:shift-y (/ margin 2)
        :block-size (+ margin (m/ceil h))
        :pos [x (m/floor (- y))]}))))


;; ;;;;;;;;;;

(defonce configuration (atom {}))

(defn- inherite-configuration
  [inheritance]
  (reduce (fn [conf k]
            (if (contains? @configuration k)
              (deep-merge conf @(@configuration k))
              conf)) {} inheritance))

(defn register-configuration!
  [chart {:keys [config doc fns] :as config-map} & inheritance]
  (swap! configuration assoc chart (delay (deep-merge (inherite-configuration inheritance) config-map))))

#_(register-configuration! :abc {:stroke {:size 3 :line 123}})
#_(register-configuration! :ded {:color :black} :abc :abc)
#_(register-configuration! :vvv {:stroke {:size 10}} :ded :abc)



;;

(defn read-json [f] (with-open [reader (io/reader f)]
                      (doall (json/read-json reader true))))


(defn read-csv [f] (rest (with-open [reader (io/reader f)]
                           (doall (csv/read-csv reader)))))

