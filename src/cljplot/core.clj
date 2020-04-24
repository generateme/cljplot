(ns cljplot.core
  (:require [cljplot.build :as b]
            [cljplot.render :as r]
            [clojure2d.core :refer [next-filename]]
            [clojure2d.extra.utils :as utils]))

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
