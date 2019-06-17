(ns cljplot.axis
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [cljplot.config :as cfg]
            [cljplot.common :refer :all]
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
    (canvas size size :high)))

(defn- axis-line
  "Draw line according to config. Canvas should be aligned to mid position."
  [canvas ^long length {:keys [color stroke angle]}]
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
      (rotate (m/radians (- ^double text-angle ^double angle))))
  (let [[_ ^double yoff _ ^double th] (text-bounding-box canvas txt)]
    (-> canvas
        (translate shift-x (int (+ (* th ^double shift-y-rel) yoff th)))
        (text txt 0 0 text-align)
        (pop-matrix))))

(defn- set-axis-font
  "Set font for ticks" 
  ([canvas font font-size style]
   (when font (set-font canvas font))
   (set-font-attributes canvas font-size style))
  ([canvas font font-size]
   (when font (set-font canvas font))
   (set-font-attributes canvas font-size)))

(defn- draw-ticks
  "Draw ticks"
  [canvas length {:keys [scale ticks fmt]} angle {:keys [size type color stroke ^double anchor font-size font
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
        mid (/ ^int (width canvas) 2)] ;; calc mid point
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
  (let [size-fn (-> config :ticks :size)]
    (max 0.0 (inc (double (-> config :line :stroke :size)))
         ^double (reduce fast-max (map #(size-fn % config) ticks)))))

(defn- find-text-bounding-box
  "Find biggest bounding box from ticks"
  [{:keys [ticks fmt]} config]
  (with-canvas [c (canvas 1 1)]
    (set-axis-font c (get-in config [:ticks :font]) (-> config :ticks :font-size))
    (reduce (fn [[^double w ^double h] t] 
              (let [s (fmt t)
                    [_ _ ^double cw ^double ch] (text-bounding-box c s)]
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

(defn- legend-shape-mark
  [canvas ^long marker-size ^long h {:keys [shape color size stroke]
                                     :or {size 6}}]
  (let [h2 (/ h 2)]
    (draw-shape canvas (/ marker-size 2) h2 shape color stroke size)))

(defn- legend-line-mark
  [canvas ^long marker-size ^long h {:keys [stroke color shape] :as conf}]
  (let [h2 (/ h 2)]
    (when stroke (set-stroke-custom canvas stroke))
    (set-color canvas color)
    (line canvas 0 h2 marker-size h2)
    (when shape (legend-shape-mark canvas marker-size h conf))))

(defn- legend-rect-mark
  [canvas ^long marker-size ^long h {:keys [stroke color] :as conf}]
  (filled-with-stroke canvas color (c/darken color) rect 0 1 marker-size (dec h)))

(defn- legend-circle-mark
  [canvas ^long marker-size ^long h {:keys [color size] :or {size 10} :as conf}]
  (set-color canvas color)
  (ellipse canvas (/ marker-size 2) (/ h 2) size size))

(defn legends-sizes
  ([name ls gap] (legends-sizes name ls gap nil 12))
  ([name ls gap font-size] (legends-sizes name ls gap nil font-size))
  ([name ls gap font font-size]
   (with-canvas [c (canvas 1 1)]
     (set-axis-font c font font-size :normal)
     (let [[^double mw ^double mh] (map #(m/ceil %) (reduce (fn [[^double w ^double h] t]
                                                              (let [[_ _ ^double cw ^double ch] (text-bounding-box c t)]
                                                                [(max w cw) (max h ch)])) [0 0] (map second ls)))]
       (set-font-attributes c font-size :bold)
       (let [[^double lw ^double lh] (map #(m/ceil %) (drop 2 (text-bounding-box c name)))]
         [(inc lh) (max mw lw) (m/ceil (+ (inc lh) ^int gap mh (* (count ls) (+ ^int gap (inc mh)))))])))))

(defn legends
  ([data] (legends data {}))
  ([data conf]
   (let [{:keys [^int gap ^int marker-size font font-size color]} (cfg/merge-configuration :legend conf)
         sizes (map (fn [[k ls]] (legends-sizes k ls gap font font-size)) data)
         
         ^int h (reduce fast+ (map last sizes))
         w (+ marker-size (* 3 gap) ^int (reduce fast-max (map second sizes)))
         ^int grouph (reduce fast-max (map first sizes))
         c (canvas (+ 50 w) (+ 50 h))]

     (with-canvas [c c]
       (translate c (+ gap 25) 25)

       (doseq [[k ls] data]

         (let [[^double sx ^double sy] (map #(m/ceil %) (text-bounding-box c k))]
           (-> (set-axis-font c font font-size :bold) 
               (set-color color)
               (push-matrix)
               (translate sx (- sy))
               (text k 0 0)
               (pop-matrix)
               (translate 0 (+ gap grouph))))

         (doseq [[type title conf] ls
                 :let [[^double sx ^double sy bw ^double bh] (map #(m/ceil %) (text-bounding-box c title))]]
           
           (case type
             :line (legend-line-mark c marker-size bh conf)
             :rect (legend-rect-mark c marker-size bh conf)
             :shape (legend-shape-mark c marker-size bh conf)
             :circle (legend-circle-mark c marker-size bh conf))

           (-> c
               (set-axis-font font font-size :normal) 
               (push-matrix)
               (translate (+ gap marker-size sx) (inc (- sy)))
               (set-color :black)
               (text title 0 0)
               (pop-matrix)
               (translate 0 (+ bh gap))))
         (translate c 0 grouph)) 
       {:canvas c
        :anchor [-25 -25]
        :block-size w}))))

;;

(defn gradient-width
  ^long [{:keys [^int gap ^int marker-size font font-size]} title mn mx]
  (with-canvas [c (canvas 1 1)]
    (set-axis-font c font font-size :bold)
    (max (text-width c title)
         (+ marker-size gap gap (max (text-width c mn)
                                     (text-width c mx))))))

(defn gradient
  [gradient w h title mn mx conf]
  (let [{:keys [^int gap ^int marker-size font font-size color]} (cfg/merge-configuration :gradient conf)
        c (canvas (+ ^long w 50) (+ ^long h 50))]
    (with-canvas [c c]
      (translate c (+ gap 25) 25)

      (let [[^double sx ^double sy bw ^double bh] (map #(m/ceil %) (text-bounding-box c title))
            gh (- ^int h bh gap)]
        (-> (set-color c color)
            (set-axis-font font font-size :bold)
            (push-matrix)
            (translate sx (- sy))
            (text title 0 0)
            (pop-matrix)
            (translate 0 (+ bh gap))
            (set-axis-font font font-size :normal)
            (push-matrix)
            (translate sx (- sy))
            (text mx (+ marker-size gap) 0)
            (translate 0 (- gh bh))
            (text mn (+ marker-size gap) 0)
            (pop-matrix))
        
        (dotimes [id gh]
          (let [gid (m/norm id 0 h 1.0 0.0)]
            (set-color c (gradient gid))
            (line c 0 id marker-size id)))))
    
    {:canvas c
     :anchor [-25 -25]}))

#_(cc/show-image (:canvas (gradient (c/gradient-presets :red-blue) 130 200 "ABBA babba" -10 20 (cfg/get-configuration :gradient))))

#_(cc/show-image (:canvas (legends {"First" [[:line "ABC" {:shape \A :color :blue}]
                                             [:line "ABCDDD" {:shape \A :color :blue}]
                                             [:line "ABCFFFFFfff" {:shape \A :color :blue}]]
                                    "Second" [[:rect "ABC" {:shape \A :color :blue}]
                                              [:rect "ABCDDD" {:shape \A :color :blue}]
                                              [:rect "ABCFFFFFfff" {:shape \A :color :blue}]]})))

#_(require '[clojure2d.extra.utils :as cc])

#_(cc/show-image (save (:canvas (legend "This is a title" [[:line "asdf" {:shape \A :color :blue}] [:line "This is longer line" {:color :maroon}] [:line 333 {:color :gray}] [:shape "Shape only" {:shape \s :color :blue}] [:rect "Rectangle" {:color :dark-olivegreen}]])) "legend1.jpg"))

#_(cc/show-image (save (:canvas (legend "This is a title" [[:circle "asdf" {:size 20 :color :blue}] [:circle "Middle circle" {:color :maroon :size 25}] [:circle 333 {:color :blue :size 30}]])) "legend2.jpg"))
