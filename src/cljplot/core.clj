(ns cljplot.core
  (:require [clojure2d.core :refer [next-filename]]
            [clojure2d.extra.utils :as utils]
            [cljplot.impl.histogram]
            [cljplot.build :as b]
            [cljplot.render :as r]
            [cljplot.impl.strips]
            [cljplot.impl.scatter]
            [cljplot.impl.line]
            [cljplot.impl.heatmap]
            [cljplot.impl.label]
            [cljplot.impl.math]
            [cljplot.impl.free]
            [cljplot.impl.time-series]))

(defn save
  "Save `chart`."
  ([chart name]
   (clojure2d.core/save chart name))
  ([chart]
   (clojure2d.core/save chart (next-filename "charts/" ".png"))))

(def show utils/show-image)

;;

(defmacro xy-chart
  [conf series & mods]
  `(-> ~series
       (b/preprocess-series)
       ~@mods
       (r/render-lattice ~conf)))
