(ns cljplot.impl.free
  (:require [cljplot.common :refer :all]))

(defmethod data-extent :free [_ _ _]
  {:x [:numerical [0 1]]
   :y [:numerical [0 1]]})

(defmethod render-graph :free [_ draw _ chart-conf]  (do-graph (assoc chart-conf :oversize 0) false (draw c)))
