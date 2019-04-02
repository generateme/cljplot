(ns cljplot.label
  (:require [cljplot.common :refer :all]
            [cljplot.core :as core]
            [cljplot.config :as conf]
            [clojure2d.core :refer :all]
            [fastmath.core :as m]))

(defmethod data-extent :label [_ _ _] nil)
(defmethod prepare-data :label [_ s conf]
  (assoc (label-size s conf) :s s))

(defmethod render-graph :label [_ {:keys [s pos]} {:keys [font font-size font-style color]} {:keys [w h orientation] :as chart-data}]
  (let [fix-orientation (assoc chart-data :orientation (case orientation
                                                         :left :right
                                                         :top :bottom
                                                         orientation))]
    (do-graph fix-orientation false
      (when font (set-font c font))
      (when font-size
        (if font-style
          (set-font-attributes c font-size font-style)
          (set-font-attributes c font-size)))
      (when (= orientation :left)
        (-> c
           (translate (/ w 2) (/ h 2))
           (rotate m/PI)
           (translate (- (/ w 2)) (- (/ h 2)))))
      (-> c
         (set-color color)
         (translate (/ w 2) 0) 
         (translate pos))
      (text c s 0 0 :center))))

(show (:canvas (let [conf (conf/merge-configuration :label {:font-size 40})
                     {:keys [size] :as data} (prepare-data :label "To jest test." conf)]
                 (render-graph :label data conf {:w 600 :h size :orientation :bottom}))))
