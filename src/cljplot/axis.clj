(ns cljplot.axis
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot.config :as cfg]
            [cljplot.common :as common]
            [clojure2d.color :as c]
            [fastmath.vector :as v]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- axis-canvas
  "Return axis canvas

  Axis canvas is much bigger than axis itself."
  [^long axis-length]
  (let [size (+ axis-length 200)]
    (c2d/canvas size size :high)))

(defn- axis-line
  "Draw line according to config. Canvas should be aligned to mid position."
  [canvas ^long length {:keys [color stroke angle]}]
  (-> canvas
      (c2d/rotate (m/radians angle))
      (c2d/translate (- (/ length 2)) 0)
      (c2d/set-color color)
      (c2d/set-stroke-custom stroke)
      (c2d/line 0 0 length 0)))

;; 

(defn- draw-tick
  "Just tick"
  [canvas type size]
  (case type
    :line (c2d/line canvas 0 0 0 size)))

(defn- draw-text
  "Draw text on canvas for given parameters."
  [canvas txt shift-x shift-y-rel text-angle text-align angle]
  (-> canvas
      (c2d/push-matrix)
      (c2d/rotate (m/radians (- ^double text-angle ^double angle))))
  (let [[_ ^double yoff _ ^double th] (c2d/text-bounding-box canvas txt)]
    (-> canvas
        (c2d/translate shift-x (int (+ (* th ^double shift-y-rel) yoff th)))
        (c2d/text txt 0 0 text-align)
        (c2d/pop-matrix))))

(defn- set-axis-font
  "Set font for ticks" 
  ([canvas font font-size style]
   (when font (c2d/set-font canvas font))
   (c2d/set-font-attributes canvas font-size style))
  ([canvas font font-size]
   (when font (c2d/set-font canvas font))
   (c2d/set-font-attributes canvas font-size)))

(defn- draw-ticks
  "Draw ticks"
  [canvas length {:keys [scale ticks fmt]} angle {:keys [size type color stroke ^double anchor font-size font
                                                         text-angle text-align shift-x shift-y-rel]}]
  (c2d/set-stroke-custom canvas stroke) ;; set ticks stroke
  (set-axis-font canvas font font-size) ;; set fonts
  (doseq [t ticks ;; for each tick
          :let [pos (m/floor (scale 0 length t)) ;; scale position
                sz (m/floor (size t))         ;; calc size
                ty (* -1.0 anchor sz)]]
    (-> canvas
        (c2d/set-color color)
        (c2d/push-matrix)
        (c2d/translate pos 0)
        (c2d/translate 0 ty)
        (draw-tick type (* anchor sz)) ;; draw tick
        (draw-text (fmt t) shift-x shift-y-rel text-angle text-align angle)
        (c2d/pop-matrix)))
  canvas)

(defn- draw-axis
  "Draw axis and ticks"
  [length scale-info config]
  (let [canvas (axis-canvas length) ;; create canvas
        mid (/ (c2d/width canvas) 2)] ;; calc mid point
    (c2d/with-canvas [c canvas]
      (-> c
          (c2d/translate mid mid) ;; go to mid
          (axis-line length (:line config))
          (draw-ticks length scale-info
                      (:angle (:line config))
                      (:ticks config))) ;; draw ticks and labels
      {:canvas canvas
       :fixed? true
       :anchor (-> (c2d/transform c 0 0) 
                   (v/fmap m/floor)
                   (v/sub)
                   (v/sub [0 1]))})))

(defn- find-max-tick-size
  "Find maximum tick size."
  [{:keys [ticks]} config]
  (let [size-fn (-> config :ticks :size)]
    (max 0.0 (inc (double (-> config :line :stroke :size)))
         ^double (reduce m/fast-max (map #(size-fn % config) ticks)))))

(defn- find-text-bounding-box
  "Find biggest bounding box from ticks"
  [{:keys [ticks fmt]} config]
  (c2d/with-canvas [c (c2d/canvas 1 1)]
    (set-axis-font c (get-in config [:ticks :font]) (-> config :ticks :font-size))
    (reduce (fn [[^double w ^double h] t] 
              (let [s (fmt t)
                    [_ _ ^double cw ^double ch] (c2d/text-bounding-box c s)]
                [(max w cw) (max h ch)])) [0 0] ticks)))

(defn- xy-axes
  [position scale-info config {:keys [w]}]
  (let [a (draw-axis w scale-info config)]
    (if (= :x position)
      (update a :anchor v/sub (v/vec2 0.0 0.0))
      (update-in a [:anchor 1] clojure.core/+ w))))

(defn axis-size
  [scale position config]
  (let [[^double bw ^double bh] (find-text-bounding-box scale config)
        ^double l (find-max-tick-size scale config)]
    (m/ceil (if (= :x position)
              (+ bh 5 l)
              (+ bw 10 l)))))

(defmethod common/data-extent :axis [_ _ _] nil)
(defmethod common/prepare-data :axis [_ d _] d)

(defmethod common/render-graph :axis [_ position config chart-data]
  (xy-axes position (:x chart-data) config chart-data))

;;;;;;;;;;;;;;;;;;;;

(defmethod common/data-extent :grid [_ _ _] nil)
(defmethod common/prepare-data :grid [_ _ _] nil)

(defmethod common/render-graph :grid [_ _ config {:keys [w h x y] :as chart-data}]
  (let [scale-x (:scale x)
        ticks-x (:ticks x)
        scale-y (:scale y)
        ticks-y (:ticks y)]
    (common/do-graph chart-data false
      (when-let [x (:x config)]
        (let [{:keys [color stroke]} x]
          (c2d/set-color c color)
          (c2d/set-stroke-custom c stroke)
          (doseq [t ticks-x
                  :let [xpos (m/floor (scale-x 0 w t))]]
            (c2d/line c xpos 0 xpos h))))

      (when-let [y (:y config)]
        (let [{:keys [color stroke]} y]
          (c2d/set-color c color)
          (c2d/set-stroke-custom c stroke)
          (doseq [t ticks-y
                  :let [ypos (inc (m/floor (scale-y 0 h t)))]]
            (c2d/line c 0 ypos w ypos)))))))


;; legends

(defn- legend-shape-mark
  [canvas ^long marker-size ^long h {:keys [shape color size stroke]
                                     :or {size 6}}]
  (let [h2 (/ h 2)]
    (common/draw-shape canvas (/ marker-size 2) h2 shape color stroke size)))

(defn- legend-line-mark
  [canvas ^long marker-size ^long h {:keys [stroke color shape] :as conf}]
  (let [h2 (/ h 2)]
    (when stroke (c2d/set-stroke-custom canvas stroke))
    (c2d/set-color canvas color)
    (c2d/line canvas 0 h2 marker-size h2)
    (when shape (legend-shape-mark canvas marker-size h conf))))

(defn- legend-rect-mark
  [canvas ^long marker-size ^long h {:keys [color]}]
  (c2d/filled-with-stroke canvas color (c/darken color) c2d/rect 0 1 marker-size (dec h)))

(defn- legend-circle-mark
  [canvas ^long marker-size ^long h {:keys [color size] :or {size 10}}]
  (c2d/set-color canvas color)
  (c2d/ellipse canvas (/ marker-size 2) (/ h 2) size size))

(defn legends-sizes
  ([name ls gap] (legends-sizes name ls gap nil 12))
  ([name ls gap font-size] (legends-sizes name ls gap nil font-size))
  ([name ls gap font font-size]
   (c2d/with-canvas [c (c2d/canvas 1 1)]
     (set-axis-font c font font-size :normal)
     (let [[^double mw ^double mh] (map #(m/ceil %)
                                        (reduce (fn [[^double w ^double h] t]
                                                  (let [[_ _ ^double cw ^double ch] (c2d/text-bounding-box c t)]
                                                    [(max w cw) (max h ch)])) [0 0] (map second ls)))]
       (c2d/set-font-attributes c font-size :bold)
       (let [[^double lw ^double lh] (map #(m/ceil %) (drop 2 (c2d/text-bounding-box c name)))]
         [(inc lh) (max mw lw) (m/ceil (+ (inc lh) ^int gap mh (* (count ls) (+ ^int gap (inc mh)))))])))))

(defn legends
  ([data] (legends data {}))
  ([data conf]
   (let [{:keys [^int gap ^int marker-size font font-size color]} (cfg/merge-configuration :legend conf)
         sizes (map (fn [[k ls]] (legends-sizes k ls gap font font-size)) data)
         
         ^int h (reduce m/fast+ (map last sizes))
         w (+ marker-size (* 3 gap) ^int (reduce m/fast-max (map second sizes)))
         ^int grouph (reduce m/fast-max (map first sizes))
         c (c2d/canvas (+ 50 w) (+ 50 h))]

     (c2d/with-canvas [c c]
       (c2d/translate c (+ gap 25) 25)

       (doseq [[k ls] data]

         (let [[^double sx ^double sy] (map #(m/ceil %) (c2d/text-bounding-box c k))]
           (-> (set-axis-font c font font-size :bold) 
               (c2d/set-color color)
               (c2d/push-matrix)
               (c2d/translate sx (- sy))
               (c2d/text k 0 0)
               (c2d/pop-matrix)
               (c2d/translate 0 (+ gap grouph))))

         (doseq [[type title conf] ls
                 :let [[^double sx ^double sy ^double _bw ^double bh] (map #(m/ceil %) (c2d/text-bounding-box c title))]]
           
           (case type
             :line (legend-line-mark c marker-size bh conf)
             :rect (legend-rect-mark c marker-size bh conf)
             :shape (legend-shape-mark c marker-size bh conf)
             :circle (legend-circle-mark c marker-size bh conf))

           (-> c
               (set-axis-font font font-size :normal) 
               (c2d/push-matrix)
               (c2d/translate (+ gap marker-size sx) (inc (- sy)))
               (c2d/set-color :black)
               (c2d/text title 0 0)
               (c2d/pop-matrix)
               (c2d/translate 0 (+ bh gap))))
         (c2d/translate c 0 grouph)) 
       {:canvas c
        :anchor [-25 -25]
        :block-size w}))))

;;

(defn gradient-width
  ^long [{:keys [^int gap ^int marker-size font font-size]} title mn mx]
  (c2d/with-canvas [c (c2d/canvas 1 1)]
    (set-axis-font c font font-size :bold)
    (max (c2d/text-width c title)
         (+ marker-size gap gap (max (c2d/text-width c mn)
                                     (c2d/text-width c mx))))))

(defn gradient
  [gradient w h title mn mx conf]
  (let [{:keys [^int gap ^int marker-size font font-size color]} (cfg/merge-configuration :gradient conf)
        c (c2d/canvas (+ ^long w 50) (+ ^long h 50))]
    (c2d/with-canvas [c c]
      (c2d/translate c (+ gap 25) 25)

      (let [[^double sx ^double sy ^double _bw ^double bh] (map #(m/ceil %) (c2d/text-bounding-box c title))
            gh (- ^int h bh gap)]
        (-> (c2d/set-color c color)
            (set-axis-font font font-size :bold)
            (c2d/push-matrix)
            (c2d/translate sx (- sy))
            (c2d/text title 0 0)
            (c2d/pop-matrix)
            (c2d/translate 0 (+ bh gap))
            (set-axis-font font font-size :normal)
            (c2d/push-matrix)
            (c2d/translate sx (- sy))
            (c2d/text mx (+ marker-size gap) 0)
            (c2d/translate 0 (- gh bh))
            (c2d/text mn (+ marker-size gap) 0)
            (c2d/pop-matrix))
        
        (dotimes [id gh]
          (let [gid (m/norm id 0 h 1.0 0.0)]
            (c2d/set-color c (gradient gid))
            (c2d/line c 0 id marker-size id)))))
    
    {:canvas c
     :anchor [-25 -25]}))

#_(cc/show-image (:canvas (gradient (c/gradient :red-blue) 130 200 "ABBA babba" -10 20 (cfg/get-configuration :gradient))))

#_(cc/show-image (:canvas (legends {"First" [[:line "ABC" {:shape \A :color :blue}]
                                             [:line "ABCDDD" {:shape \A :color :blue}]
                                             [:line "ABCFFFFFfff" {:shape \A :color :blue}]]
                                    "Second" [[:rect "ABC" {:shape \A :color :blue}]
                                              [:rect "ABCDDD" {:shape \A :color :blue}]
                                              [:rect "ABCFFFFFfff" {:shape \A :color :blue}]]})))

#_(require '[clojure2d.extra.utils :as cc])

#_(cc/show-image (:canvas (common/legend "This is a title" [[:line "asdf" {:shape \A :color :blue}] [:line "This is longer line" {:color :maroon}] [:line 333 {:color :gray}] [:shape "Shape only" {:shape \s :color :blue}] [:rect "Rectangle" {:color :dark-olivegreen}]])) "legend1.jpg")

#_(cc/show-image (save (:canvas (legend "This is a title" [[:circle "asdf" {:size 20 :color :blue}] [:circle "Middle circle" {:color :maroon :size 25}] [:circle 333 {:color :blue :size 30}]])) "legend2.jpg"))
