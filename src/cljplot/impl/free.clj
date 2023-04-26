(ns cljplot.impl.free
  (:require [cljplot.common :as common]))

(defmethod common/data-extent :free [_ _ _] nil)

(defmethod common/render-graph :free [_ draw conf {:keys [orientation] :as chart-conf}]
  (common/do-graph (assoc chart-conf
                          :oversize 0
                          :orientation (or orientation :bottom)) false (draw c conf chart-conf)))
