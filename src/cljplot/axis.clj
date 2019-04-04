(ns cljplot.axis
  (:require [clojure2d.core :refer :all]
            [clojure2d.extra.utils :as utils]
            [fastmath.stats :as stats]
            [fastmath.core :as m]
            [cljplot.scale :as s]
            [cljplot.config :as cfg]
            [cljplot.common :refer :all]
            [clojure2d.color :as c]
            [fastmath.vector :as v]
            [fastmath.random :as r]))

(defn- axis-canvas
  "Return axis canvas

  Axis canvas is much bigger than axis itself."
  [axis-length]
  (let [size (+ axis-length 200)]
    (canvas size size :high)))

(defn- axis-line
  "Draw line according to config. Canvas should be aligned to mid position."
  [canvas length {:keys [color stroke angle]}]
  (-> canvas
      (rotate (m/radians angle))
      (translate (- (/ length 2)) 0)
      (set-color color)
      (set-stroke-custom stroke)
      (line 0 0 length 0)))

;; 

(defn- draw-tick
  "Just tick"
  [canvas type size]
  (case type
    :line (line canvas 0 0 0 size)))

(defn- draw-text
  "Draw text on canvas for given parameters."
  [canvas txt shift-x shift-y-rel text-angle text-align angle]
  (-> canvas
      (push-matrix)
      (rotate (m/radians (- text-angle angle))))
  (let [[_ yoff _ th] (text-bounding-box canvas txt)]
    (-> canvas
        (translate shift-x (+ (* th shift-y-rel) yoff th))
        (text txt 0 0 text-align)
        (pop-matrix))))

(defn- set-axis-font
  "Set font for ticks"
  [canvas font font-size]
  (when font (set-font canvas font))
  (set-font-attributes canvas font-size))

(defn- draw-ticks
  "Draw ticks"
  [canvas length {:keys [scale ticks fmt]} angle {:keys [size type color stroke anchor font-size font
                                                         text-angle text-align shift-x shift-y-rel]}]
  (set-stroke-custom canvas stroke) ;; set ticks stroke
  (set-axis-font canvas font font-size) ;; set fonts
  (doseq [t ticks ;; for each tick
          :let [pos (m/floor (scale 0 length t)) ;; scale position
                sz (m/floor (size t))         ;; calc size
                ty (* -1.0 anchor sz)]]
    (-> canvas
       (set-color color)
       (push-matrix)
       (translate pos 0)
       (translate 0 ty)
       (draw-tick type (* anchor sz)) ;; draw tick
       (draw-text (fmt t) shift-x shift-y-rel text-angle text-align angle)
       (pop-matrix)))
  canvas)

(defn- draw-axis
  "Draw axis and ticks"
  [length scale-info config]
  (let [canvas (axis-canvas length) ;; create canvas
        mid (/ (width canvas) 2)] ;; calc mid point
    (with-canvas [c canvas]
      (-> c
          (translate mid mid) ;; go to mid
          (axis-line length (:line config))
          (draw-ticks length scale-info
                      (:angle (:line config))
                      (:ticks config))) ;; draw ticks and labels
      {:canvas canvas
       :fixed? true
       :anchor (-> (transform c 0 0) 
                   (v/fmap m/floor)
                   (v/sub)
                   (v/sub [0 1]))})))

(defn- find-max-tick-size
  "Find maximum tick size."
  [{:keys [ticks]} config]
  (let [size-fn (get-in config [:ticks :size])]
    (max 0 (inc (get-in config [:line :stroke :size]))
         (reduce max (map #(size-fn % config) ticks)))))

(defn- find-text-bounding-box
  "Find biggest bounding box from ticks"
  [{:keys [ticks fmt]} config]
  (with-canvas [c (canvas 1 1)]
    (set-axis-font c (get-in config [:ticks :font]) (get-in config [:ticks :font-size]))
    (reduce (fn [[w h] t] 
              (let [s (fmt t)
                    [_ _ cw ch] (text-bounding-box c s)]
                [(max w cw) (max h ch)])) [0 0] ticks)))

(defn- xy-axes
  [position scale-info config {:keys [w]}]
  (let [a (draw-axis w scale-info config)]
    (if (= :x position)
      (update a :anchor v/sub (v/vec2 0.0 1.0))
      (update-in a [:anchor 1] + w))))

(defn axis-size
  [scale position config]
  (let [[bw bh] (find-text-bounding-box scale config)
        l (find-max-tick-size scale config)]
    (m/ceil (if (= :x position)
              (+ bh 5 l)
              (+ bw 10 l)))))

(defmethod data-extent :axis [_ _ _] nil)
(defmethod prepare-data :axis [_ d _] d)

(defmethod render-graph :axis [_ position config chart-data]
  (xy-axes position (:x chart-data) config chart-data))

;;;;;;;;;;;;;;;;;;;;

(defmethod data-extent :grid [_ _ _] nil)
(defmethod prepare-data :grid [_ _ _] nil)

(defmethod render-graph :grid [_ _ config {:keys [w h x y] :as chart-data}]
  "" 
  (let [scale-x (:scale x)
        ticks-x (:ticks x)
        scale-y (:scale y)
        ticks-y (:ticks y)]
    (do-graph chart-data false
      (when-let [x (:x config)]
        (let [{:keys [color stroke]} x]
          (set-color c color)
          (set-stroke-custom c stroke)
          (doseq [t ticks-x
                  :let [xpos (m/floor (scale-x 0 w t))]]
            (line c xpos 0 xpos h))))

      (when-let [y (:y config)]
        (let [{:keys [color stroke]} y]
          (set-color c color)
          (set-stroke-custom c stroke)
          (doseq [t ticks-y
                  :let [ypos (inc (m/floor (scale-y 0 h t)))]]
            (line c 0 ypos w ypos)))))))


;; legends

(def ^:private ^:const ^int legend-gap 5)
(def ^:private ^:const ^int legend-gap3 (* 3 legend-gap))
(def ^:private ^:const ^int mark-size 40)

(defn- legend-shape-mark
  [canvas h {:keys [shape color]}]
  (let [h2 (/ h 2)]
    (println shape)
    (draw-shape canvas (/ mark-size 2) h2 shape color nil 6)))

(defn- legend-line-mark
  [canvas h {:keys [stroke color shape] :as conf}]
  (let [h2 (/ h 2)]
    (set-color canvas color)
    (line canvas 0 h2 mark-size h2)
    (when shape (legend-shape-mark canvas h conf))))

(defn- legend-rect-mark
  [canvas h {:keys [stroke color] :as conf}]
  (set-color canvas color)
  (rect canvas 0 0 mark-size h))


(defn legend
  [name data]
  (let [txts (map second data)
        txts (if name (conj txts name) txts)
        [mw mh] (with-canvas [c (canvas 1 1)]
                  (set-font-attributes c 12 :normal)
                  (map #(m/ceil %) (reduce (fn [[w h] t] 
                                             (let [[_ _ cw ch] (text-bounding-box c t)]
                                               [(max w cw) (max h ch)])) [0 0] txts)))
        
        h (+ legend-gap legend-gap (* (count txts) (inc mh)))
        w (+ legend-gap3 mw mark-size)
        c (canvas (+ 50 w) (+ 50 h) :highest)]

    (with-canvas [c c]
      (translate c (+ 25 legend-gap) (+ 25 legend-gap))

      (when name
        (let [[sx sy] (map #(m/ceil %) (text-bounding-box c name))]
          (-> c
              (set-color :black)
              (push-matrix)
              (translate sx (- sy))
              (set-font-attributes 12 :bold)
              (text name 0 0)
              (pop-matrix)
              (translate 0 (+ legend-gap legend-gap mh)))))

      (set-font-attributes c 12 :normal)
      (doseq [[type title conf] data
              :let [[sx sy bw bh] (map #(m/ceil %) (text-bounding-box c title))]]

        (case type
          :line (legend-line-mark c bh conf)
          :rect (legend-rect-mark c bh conf))

        (-> c
            (push-matrix)
            (translate (+ legend-gap mark-size sx) (- sy))
            (set-color :black)
            (text title 0 0)
            (pop-matrix)
            (translate 0 (inc mh))))
      {:canvas c
       :anchor [25 25]})))

;;(require '[cljplot.core :as cc])

;;(cc/show (:canvas (legend "Blah" [[:line "asdf" {:shape \A :color :blue}] [:line "This is test of longer line" {:color :maroon}] [:line 333 {:color :gray}]])))
