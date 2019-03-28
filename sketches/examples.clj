(ns cljplot.sketches.examples
  (:require [cljplot.render :as r]
            [cljplot.build :as b]
            [cljplot.common :refer :all]
            [fastmath.interpolation :as in]
            [fastmath.stats :as stats]
            [clojure2d.color :as c]
            [cljplot.scale :as s]
            [fastmath.core :as m]
            [fastmath.random :as rnd]
            [cljplot.core :refer :all]
            [java-time :as dt]
            [clojure.string :as str]
            [clojure2d.core :as c2d]
            [clojure2d.pixels :as p]
            [fastmath.vector :as v]))

;; logo

(let [logo (p/to-pixels (c2d/with-oriented-canvas-> :bottom-left+ (c2d/canvas 200 100)
                          (c2d/set-font "Raleway Thin")
                          (c2d/set-background :black)
                          (c2d/set-color :white)
                          (c2d/set-font-attributes 60)
                          (c2d/text "cljplot" 20 70)))
      gradient (c/gradient-presets :two-heads-filonov)
      side-conf {:color (nth (iterate c/brighten (gradient 0.1)) 3) :area? true :margins {:x [0.02 0.02]}}
      pairs (->> (repeatedly #(v/vec2 (rnd/irand 200) (rnd/irand 100)))
               (filter #(pos? (c/luma (apply p/get-color logo %))))
               (map #(v/add % (v/generate-vec2 rnd/grand)))
               (take 3000))]
  (-> (b/series [:grid]
               [:scatter pairs {:size (fn [_ _] (* 10 (m/pow (rnd/drand 0.1 1.0) 3)))
                                :color (fn [[x _] conf]
                                         (let [[mn mx] (get-in conf [:extent :x 1])]
                                           (c/set-alpha (gradient (m/norm x mn mx)) 50)))}])
     (b/preprocess-series)
     (b/add-side :top 25 (b/series [:density (map first pairs) side-conf]))
     (b/add-side :right 25 (b/series [:density (map second pairs) side-conf]))
     (b/add-axes :bottom)
     (b/add-axes :left)
     (r/render-lattice {:width 600 :height 300})
     (save "results/examples/logo.jpg")
     (show)))

;;
