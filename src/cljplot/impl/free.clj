(ns cljplot.impl.free
  (:require [cljplot.common :refer :all]))

(defmethod data-extent :free [_ _ _] nil)
(defmethod render-graph :free [_ draw conf chart-conf]  (do-graph (assoc chart-conf :oversize 0) false (draw c conf chart-conf)))
