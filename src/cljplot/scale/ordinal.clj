(ns cljplot.scale.ordinal
  (:require [cljplot.scale.common :as sc]))

(defn ordinal
  ([xs] (ordinal xs :ordinal))
  ([xs type] (ordinal xs type nil))
  ([xs type info]
   (let [cnt (count xs)
         d (range cnt)
         r (vec xs)]
     (sc/->OrdinalScale d r cnt type r (zipmap xs d) info))))

(defmethod sc/ticks :ordinal [s & [c]]
  (sc/ordinal-ticks s c))
