(ns cljplot.core
  (:require [clojure2d.core :refer [next-filename]]
            [cljplot.common :refer :all]
            [clojure2d.extra.utils :as utils]
            [cljplot.impl.histogram :refer :all]
            [cljplot.build :as b]
            [cljplot.render :as r]
            [cljplot.impl.strips :refer :all]
            [cljplot.impl.scatter :refer :all]
            [cljplot.impl.line :refer :all]))

(defn save
  "Save `chart`."
  ([chart name]
   (clojure2d.core/save chart name))
  ([chart]
   (clojure2d.core/save chart (next-filename "charts/" ".png"))))

(def show utils/show-image)

