(ns cljplot.impl.label
  (:require [cljplot.common :refer :all]
            [cljplot.config :as conf]
            [clojure2d.core :refer :all]
            [fastmath.core :as m]))

(defmethod data-extent :label [_ _ _] nil)
(defmethod prepare-data :label [_ s conf]
  (assoc (label-size s conf) :s s :conf conf))

(defmethod render-graph :label [_ {:keys [s pos shift-y]} {:keys [font font-size font-style color]} {:keys [w h orientation] :as chart-data}]
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

      (when (= orientation :right)
        (-> c
            (translate (/ w 2) (/ h 2))
            (rotate m/PI)
            (translate (- (/ w 2)) (- (/ h 2)))))
      (-> c
          (translate (/ w 2) shift-y) 
          (translate pos)
          (set-color color)
          (text s 0 0 :center)))))

#_(core/show (:canvas (let [conf (conf/merge-configuration :label {:font-size 40})
                            {:keys [size] :as data} (prepare-data :label "This is test." conf)]
                        (render-graph :label data conf {:w 600 :h size :orientation :bottom}))))
