(ns cljplot.impl.label
  (:require [cljplot.common :as common]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defmethod common/data-extent :label [_ _ _] nil)
(defmethod common/prepare-data :label [_ s conf]
  (assoc (common/label-size s conf) :s s :conf conf))

(defmethod common/render-graph :label [_ {:keys [s pos shift-y]} {:keys [font font-size font-style color]} {:keys [^int w ^int h orientation] :as chart-data}]
  (let [fix-orientation (assoc chart-data :orientation (case orientation
                                                         :left :right
                                                         :top :bottom
                                                         orientation))]
    (common/do-graph fix-orientation false
      (when font (c2d/set-font c font))
      (when font-size
        (if font-style
          (c2d/set-font-attributes c font-size font-style)
          (c2d/set-font-attributes c font-size)))

      (when (= orientation :right)
        (-> c
            (c2d/translate (/ w 2) (/ h 2))
            (c2d/rotate m/PI)
            (c2d/translate (- (/ w 2)) (- (/ h 2)))))
      (-> c
          (c2d/translate (/ w 2) shift-y) 
          (c2d/translate pos)
          (c2d/set-color color)
          (c2d/text s 0 0 :center)))))

(m/unuse-primitive-operators)
