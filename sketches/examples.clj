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
            [fastmath.complex :as cx]
            [fastmath.fields :as f]
            [fastmath.vector :as v]))

;; logo

(rnd/set-seed! rnd/default-rng 2)

(def logo (p/to-pixels (c2d/with-oriented-canvas-> :bottom-left+ (c2d/canvas 200 100)
                         (c2d/set-font "Raleway Thin")
                         (c2d/set-background :black)
                         (c2d/set-color :white)
                         (c2d/set-font-attributes 60)
                         (c2d/text "cljplot" 20 70))))

(let [gradient (c/gradient-presets :two-heads-filonov)
      side-conf {:color (nth (iterate c/brighten (gradient 0.1)) 3) :area? true :margins {:x [0.02 0.02]}}
      pairs (->> (repeatedly #(v/vec2 (rnd/irand 200) (rnd/irand 100)))
                 (filter #(pos? (c/luma (apply p/get-color logo %))))
                 (map #(v/add % (v/generate-vec2 rnd/grand)))
                 (take 2500))]
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
      (b/add-label :bottom "cljplot charting library")
      (r/render-lattice {:width 600 :height 300})
      (save "results/examples/logo.jpg")
      (show)))

;; single line charts

(let [blue (last (:rdylbu-9 c/palette-presets))
      red (first (:rdylbu-9 c/palette-presets))
      data (repeatedly 2000 (fn [] (rnd/randval (rnd/grand) (rnd/randval 0.3 (rnd/grand -10 2) (rnd/grand 10 2)))))
      g [:grid nil {:y nil}]]
  (-> (b/series [:grid] [:violin data])
      (b/preprocess-series)
      
      (b/add-side :top 30 (b/series g [:strip data {:color (c/set-alpha red 30) :distort 0.2}]))
      (b/add-side :top 30 (b/series g [:rug data]))
      (b/add-side :top 30 (b/series g [:box data]))
      (b/add-side :top 30 (b/series g [:histogram data {:type :lollipops :bins 100 :palette [blue]}]))
      (b/add-side :top 30 (b/series g [:extent-stat data {:color :black}]))
      
      (b/add-side :bottom 30 (b/series g [:density data {:area? true :margins nil}]))
      (b/add-side :bottom 50 (b/series g [:histogram data {:stroke? false :bins 150 :palette [blue]}]))

      (b/update-scales :y :ticks 5)
      
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-axes :right)
      (b/add-axes :top)

      (b/add-side :left (b/series [:label "violin"]))
      (b/add-side :right (b/series [:label "violin"]))

      (b/add-side :top (b/series [:label "extent"]))
      (b/add-side :bottom (b/series [:label "extent"]))

      (b/add-label :top "Stacked 1d/2d charts" {:color (c/darken :steelblue) :font-size 20})
      
      (r/render-lattice {:width 800 :height 500 :padding-in 0.0})
      (save "results/examples/strips.jpg")
      (show)))

;;

(let [data (repeatedly 600000 #(vector (rnd/grand) (rnd/grand)))]
  (-> (b/series [:cloud data {:kernel :gaussian :logarithmic? false :gradient (c/gradient-presets :tornyai)}]
                [:grid])
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (r/render-lattice {:width 800 :height 555})
      (save "results/examples/point-cloud.jpg")
      (show)))

(let [data (repeatedly 10000 #(rnd/randval [(rnd/grand) (rnd/grand)]
                                           [(rnd/grand -10 1) (rnd/grand -10 1)]))]
  (-> (b/series [:grid] [:heatmap data {:grid :flat-hex :alpha-factor 0 :size 20 :gradient (c/gradient-presets :prl-2)}])
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (r/render-lattice {:width 600 :height 600})
      (save "results/examples/heatmap.jpg")
      (show)))

;;

(-> (b/series [:complex #(cx/div (cx/log %) (cx/sin %)) {:x [-6 6] :y [-6 6] :colorspace :HSI :wrap-method :log2}])
   (b/preprocess-series)
   (b/add-axes :bottom)
   (b/add-axes :left)
   (b/add-label :bottom "log(z)/sin(z)")
   (r/render-lattice {:width 600 :height 600})
   (save "results/examples/complex.jpg")
   (show)) 

(-> (b/series [:scalar #(apply rnd/vnoise %)  {:x [-6 6] :y [-6 6]}])
   (b/preprocess-series)
   (b/add-axes :bottom)
   (b/add-axes :left)
   (b/add-label :bottom "value noise")
   (r/render-lattice {:width 600 :height 600})
   (save "results/examples/vnoise.jpg")
   (show)) 

(let [field (f/combine {:type :operation, :name :comp, :var1 {:type :operation, :name :comp, :var1 {:type :variation, :name :secant, :amount 1.0, :config {}}, :var2 {:type :variation, :name :diamond, :amount 1.0, :config {}}, :amount 1.0}, :var2 {:type :variation, :name :power, :amount 1.0, :config {}}, :amount 1.0})]
  (-> (b/series [:grid] [:field field  {:jitter 1.0 :x [-4 4] :y [-4 4] :wrap? false}])
     (b/preprocess-series)
     (b/add-axes :bottom)
     (b/add-axes :left)
     (b/add-label :bottom "vector field")
     (r/render-lattice {:width 600 :height 600})
     (save "results/examples/field.jpg")
     (show))) 

;;

(let [field (f/field :hyperbolic)
      sfield (f/heading field)]
  (-> (b/series [:grid]
                [:scalar sfield {:wrap-method :sin}]
                [:vector field {:grid :shifted-square :scale 1.5}])
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "hyperbolic")
      (r/render-lattice {:width 600 :height 600})
      (save "results/examples/vector-field.jpg")
      (show)))


(let [field (f/field :exp)]
  (-> (b/series [:grid]
               [:trace field {:x [-7 7] :y [-7 7]}])
     (b/preprocess-series)
     (b/add-axes :bottom)
     (b/add-axes :left)
     (b/add-label :bottom "exponential")
     (r/render-lattice {:width 600 :height 600})
     (save "results/examples/field-trace.jpg")
     (show)))

(def faithful (read-json "data/faithful.json"))

(let [data (map (juxt :waiting :eruptions) faithful)]
  (-> (b/series [:grid nil {:position [0 0]}]
               [:grid nil {:position [1 0]}])
     (b/add-serie [:density-2d data {:blur-kernel-size 50 :contours 20}] 0 0)
     (b/add-serie [:scatter data {:label "Kernel size 50, contours 20"}] 0 0)
     (b/add-serie [:density-2d data {:blur-kernel-size 10}] 1 0)
     (b/add-serie [:scatter data {:label "Kernel size 10, contours 10"}] 1 0)
     (b/preprocess-series)
     (b/add-axes :bottom)
     (b/add-axes :left)
     (b/add-label :bottom "Waiting time (minutes)" {:font "Liberation Mono"})
     (b/add-label :left "Eruption time (minutes)" {:font "Liberation Mono"})
     (r/render-lattice {:width 900 :height 500})
     (save "results/examples/contour.jpg")
     (show)))
