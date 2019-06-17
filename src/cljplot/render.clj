(ns cljplot.render
  (:require [cljplot.scale :as s]
            [cljplot.common :refer :all]
            [cljplot.axis :as ax]
            [clojure2d.core :refer :all] 
            [fastmath.core :as m]
            [fastmath.vector :as v]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- get-pos-size
  "Sum sizes of all charts in given direction."
  [block]
  (reduce clojure.core/+ 0 (map :size block)))

(defn- get-max-size
  [side]
  (reduce clojure.core/max 0 (map get-pos-size (vals side))))

(defmacro place-image
  [c canv anch px py]
  `(-> ~c
       (push-matrix)
       (translate ~anch)
       (image ~canv ~px ~py)
       (pop-matrix)))

(defn- place-sides
  [c series scale [^int tx ^int ty] orientation axis bands]
  (let [x? (= :x axis)]
    (doseq [[pos srs] series
            :let [[start ssize] (bands pos)
                  [imgx imgy] (if x? [start 0] [0 start])]]
      (push-matrix c)
      (doseq [{:keys [^double size series]} srs]
        (doseq [[t d conf] series
                :let [sx (scale pos)
                      ;; construct y scale
                      sy (or (:scale-y conf) (s/scale-map [:linear] {:domain (-> conf :extent :y second)}))
                      ;; render graph
                      {:keys [canvas fixed? anchor]} (render-graph t d conf
                                                                   {:orientation orientation :w ssize :h size :x sx :y sy})
                      ;; axis has already correct anchor
                      anchor (if fixed? anchor
                                 (case orientation
                                   :top (v/sub anchor [0 size])
                                   :left (v/sub anchor [size 0])
                                   anchor))]]
          (place-image c canvas anchor imgx imgy))
        (translate c (* tx size) (* ty size)))
      (pop-matrix c))
    c))

;; inner part
(defn- render-lattice-inner
  [c {:keys [rows cols series extents scales sizes left right top bottom] :as srs}
   {:keys [padding-in padding-out ^int width ^int height]
    :or {padding-in 0.05 padding-out 0.0}}]   
  (let [[^long l ^long r ^long t ^long b] (map (comp get-max-size srs) [:left :right :top :bottom])
        ww (- width l r)
        hh (- height t b)
        bands-conf {:padding-in padding-in :padding-out padding-out}
        bands-x (bands->positions-size (s/bands bands-conf cols) ww)
        bands-y-raw (s/bands bands-conf (reverse (range rows)))
        bands-y (bands->positions-size bands-y-raw hh)
        scale-x (:x scales)
        scale-y (:y scales)] 

    ;; top left corner
    (translate c l t)
    
    ;; draw lattice
    (doseq [[^int x ^int y :as id] (keys series)
            :let [[start-x w] (bands-x x)
                  [start-y h] (bands-y y)]]

      ;; shades
      (when (odd? (+ x y))
        (set-color c :black 20)
        (rect c start-x start-y w h))
      
      (doseq [[t d conf] (series id) 
              :let [sx (scale-x x)
                    sy (scale-y y)
                    ex (-> extents :x (get x) second)
                    ey (-> extents :y (get y) second)
                    {:keys [canvas anchor]} (render-graph t d conf {:w w :h h :x sx :y sy :extent {:x ex :y ey}})]]

        (place-image c canvas (v/add anchor [0 1]) start-x start-y)
        
        (when-let [label (:label conf)]
          (let [lc (render-label label w)]
            (image c lc start-x start-y)))))

    ;; draw sides/axes
    (-> c
        (place-sides left scale-y [-1 0] :left :y bands-y)
        (place-sides top scale-x [0 -1] :top :x bands-x)
        (push-matrix)
        (translate ww 0)
        (place-sides right scale-y [1 0] :right :y bands-y)
        (pop-matrix)
        (push-matrix)
        (translate 0 hh)
        (place-sides bottom scale-x [0 1] :bottom :x bands-x)
        (pop-matrix))

    [l r t b ww hh]))

(defn- place-label
  [c label px py conf o w h]
  (when label
    (let [{:keys [canvas anchor]} (render-graph :label label (:conf label)
                                                (assoc conf :orientation o :w w :h h))]
      (place-image c canvas anchor px py))))

;; outer part (labels, legends, gradients)
(defn render-lattice
  ([srs] (render-lattice srs {}))
  ([{:keys [labels legend gradient] :or {labels {}} :as srs}
    {:keys [^int width ^int height background ^int border]
     :or {width 800 height 800 background 0xe8e8f0 border 10}
     :as conf}]

   (let [{:keys [left right top bottom]} labels
         [^int l ^int r ^int t ^int b] (map (comp #(or % 0) :block-size) [left right top bottom])
         
         legend (when legend (ax/legends legend))
         ^int legend-width (or (:block-size legend) 0)
         
         tl (+ border l)
         tt (+ border t)
         ww (- width tl r border legend-width)
         hh (- height tt b border)]
     (with-canvas [c (canvas width height)]
       
       (if (satisfies? ImageProto background)
         (image c background)
         (set-background c background))

       (push-matrix c)
       (translate c tl tt)
       
       ;; labels
       ;; take inner chart position and sides
       (let [[^int il ^int ir ^int it ^int ib ^int iw ^int ih] (render-lattice-inner c srs (assoc conf :width ww :height hh))]
         
         (pop-matrix c)
         (when legend (place-image c (:canvas legend) (:anchor legend) (+ tl ww) (+ it tt)))
         (place-label c left border (+ it tt) conf :left ih l)
         (place-label c top (+ il tl) border conf :top iw t)
         (place-label c bottom (+ il tl) (+ tt hh) conf :bottom iw b)
         (place-label c right (+ tl ww legend-width) (+ it tt) conf :right ih r)) 

       c))))
