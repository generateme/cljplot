(ns cljplot.render
  (:require [cljplot.scale :as s]
            [cljplot.common :refer :all]
            [clojure2d.core :refer :all]
            [clojure2d.extra.utils :as utils]
            [cljplot.build :as b]
            [fastmath.core :as m]
            [fastmath.vector :as v]))

(defn- get-pos-size
  "Sum sizes of all charts in given direction."
  [block]
  (reduce + 0 (map :size block)))

(defn- get-max-size
  [side]
  (reduce max 0 (map get-pos-size (vals side))))

(defmacro place-image
  [c canv anch px py]
  `(-> ~c
       (push-matrix)
       (translate ~anch)
       (image ~canv ~px ~py)
       (pop-matrix)))

(defn- place-sides
  [c series scale [tx ty] orientation axis bands]
  (let [x? (= :x axis)]
    (doseq [[pos srs] series
            :let [[start ssize] (bands pos)
                  [imgx imgy] (if x? [start 0] [0 start])]]
      (push-matrix c)
      (doseq [{:keys [size series]} srs]
        (doseq [[t d conf] series
                :let [sx (scale pos)
                      ;; construct y scale
                      sy (or (:scale-y conf) (s/scale-map [:linear] {:domain (->> conf :extent :y second)}))
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

(defn render-lattice
  ([srs] (render-lattice srs nil))
  ([{:keys [rows cols series scales sizes left right top bottom] :as srs}
    {:keys [padding-in padding-out width height background border]
     :or {padding-in 0.05 padding-out 0.0 width 800 height 800 background 0xe8e8f0 border 15}}]   
   (let [[l r t b] (map (comp get-max-size srs) [:left :right :top :bottom])
         ww (- width l r border border)
         hh (- height t b border border)
         bands-conf {:padding-in padding-in :padding-out padding-out}
         bands-x (bands->positions-size (s/bands bands-conf cols) ww)
         bands-y-raw (s/bands bands-conf (reverse (range rows)))
         ;;         gap (* hh (- (:step (:info bands-y-raw)) (:bandwidth (:info bands-y-raw))))
         bands-y (bands->positions-size bands-y-raw hh)
         scale-x (:x scales)
         scale-y (:y scales)] 
     (with-canvas [c (canvas width height)]

       ;; draw background or set color
       (if (satisfies? ImageProto background)
         (image c background)
         (set-background c background))

       ;; top left corner
       (translate c (+ border l) (+ border t))
       
       ;; draw lattice
       (doseq [[x y :as id] (keys series)
               :let [[start-x w] (bands-x x)
                     [start-y h] (bands-y y)]]

         ;; shades
         (when (odd? (+ x y))
           (set-color c :black 20)
           (rect c start-x start-y w h))
         
         (doseq [[t d conf] (series id) 
                 :let [sx (scale-x x)
                       sy (scale-y y)
                       {:keys [canvas anchor]} (render-graph t d conf {:w w :h h :x sx :y sy})]]
           
           (place-image c canvas anchor start-x start-y)
           (-> c
              (push-matrix)
              (translate anchor)
              (image canvas start-x start-y)
              (pop-matrix))
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
          (pop-matrix))))))

