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
            [fastmath.vector :as v]
            [fastmath.gp :as gp]
            [fastmath.distance :as dist]
            [fastmath.kernel :as kk]))

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

;; lag

(def mc (take 10000 (iterate #(+ (* 0.95 %) (rnd/grand) (rnd/randval 0.01 10 0)) 0.0)))

(-> (b/series [:grid] [:lag mc {:lag 7 :color (c/color :black 30)}])
    (b/preprocess-series)
    (b/add-axes :bottom)
    (b/add-axes :left)
    (b/add-label :bottom "y(t)")
    (b/add-label :left "y(t+7)")
    (r/render-lattice {:width 600 :height 600})
    (save "results/examples/lag.jpg")
    (show))

;; ACF/PACF

;; http://people.math.sfu.ca/~lockhart/richard/804/06_1/lectures/IdentExamples/web.pdf

(def ep (repeatedly 10000 rnd/grand))
(def ar1 (drop 9500 (reductions (fn [c r]
                                  (+ r (* -0.99 c))) (first ep) (rest ep))))
(def ar2 (drop 9500 (map first (reductions (fn [[c1 c2] r]
                                             [(+ r c1 (* -0.99 c2)) c1]) [(first ep) (second ep)] (drop 2 ep)))))
(def ar3 (drop 9500 (map first (reductions (fn [[c1 c2 c3] r]
                                             [(+ r (* 0.8 c1) (/ c2 -3.0) (* 0.4672897196261683 c3)) c1 c2]) [(first ep) (second ep) (nth ep 2)] (drop 3 ep)))))
(def ma2 (take 500 (map (fn [e1 e2 e3] (+ (- e1) (* 0.8 e2) (* -0.9 e3))) (drop 2 ep) (drop 1 ep) ep)))
(def sunspots (map :x (read-json "data/sunspot_year.json")))

;; (/ 0.8 1.712)
;; => 0.4672897196261683

(let [data ma2]
  (-> (b/series [:grid] [:line (map-indexed vector data)])
      (b/preprocess-series)
      (b/update-scales :x :fmt int)
      (b/add-axes :bottom)
      (b/add-axes :left)
      ;; (b/add-side :right (b/series [:grid nil {:y nil}] [:histogram data {:bins 20}]))
      (r/render-lattice {:width 600 :height 200})
      (save "results/examples/ma2.jpg")
      (show))

  (-> (b/series [:grid nil {:position [0 1]}] [:acf data {:lags 50 :position [0 1] :label "ACF MA(2)"}]
                [:grid nil {:position [0 0]}] [:pacf data {:lags 50 :position [0 0] :label "PACF MA(2)"}])
      (b/preprocess-series)
      (b/update-scales :x :fmt int)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "lag")
      (b/add-label :left "autocorrelation")
      (r/render-lattice {:width 600 :height 400})
      (save "results/examples/acf.jpg")
      (show)))

;;

;; qqplot/ppplot

(-> (xy-chart {:width 600 :height 600}
              (b/series [:grid]
                        [:abline]
                        [:ppplot [(rnd/distribution :t) (repeatedly 40000 rnd/grand)] {:domain [-3 3]}])
              (b/add-axes :bottom)
              (b/add-axes :left)
              (b/add-label :bottom "ppplot t-student vs normal"))
    (save "results/examples/ppplot.jpg")
    (show))

(-> (xy-chart {:width 600 :height 600}
              (b/series [:grid]
                        [:abline]
                        [:vline]
                        [:hline]
                        [:qqplot [(rnd/distribution :t) (repeatedly 40000 rnd/grand)]])
              (b/add-axes :bottom)
              (b/add-axes :left)
              (b/add-label :bottom "qqplot t-student vs normal"))
    (save "results/examples/qqplot.jpg")
    (show))


;;;

(let [data (concat (repeatedly 500 #(rnd/grand 4.5 0.1))
                   (take 10000 (remove #(== (m/round (* % 10.0)) 30.0) (repeatedly #(+ (rnd/grand 2.0 0.1) (* (rnd/drand) (rnd/drand 1.0 3.0)))))))
      fu (stats/kernel-density :uniform data)
      fs (stats/kernel-density :gaussian data)
      m (keys (methods stats/kernel-density))
      kdes (zipmap m (map #(stats/kernel-density % data) m))]
  (-> (xy-chart {:width 600 :height 600}
                (-> (b/series [:grid] )
                    (b/add-multi :function kdes
                                 {:points 600 :domain [1.0 6.0] :stroke {:size 1.5}} {:color (cycle (map #(c/set-alpha % 200) (c/palette-presets :category10)))}))
                (b/add-axes :bottom)
                (b/add-axes :left)
                (b/add-label :bottom "Various kernel densities"))
      (save "results/examples/kernel-densities.jpg")
      (show)))


;;


(defn sinf [v] (+ (rnd/grand 0.00005) (+ 10 (m/sin (- (* 0.7 v))))))

(time (let [N 100
            n 10
            xs (repeatedly n #(rnd/drand -5 5))
            ys (map sinf xs)
            gp (gp/gaussian-process xs ys {:normalize? true :kernel
                                           (kk/kernel :gaussian)})
            xtest (map #(m/norm % 0 (dec N) -5.0 5.0) (range N))
            [mu stddev] (gp/predict gp xtest true)
            s95 (map (partial * 1.96) stddev)
            s50 (map (partial * 0.67) stddev)]
        (-> (xy-chart {:width 800 :height 600}
                      (b/series [:grid]
                                [:ci [(map vector xtest (map - mu s95)) (map vector xtest (map + mu s95))] {:color (c/color :lightblue 180)}]
                                [:ci [(map vector xtest (map - mu s50)) (map vector xtest (map + mu s50))] {:color (c/color :lightblue)}]
                                [:function sinf {:domain [-5 5] :color :red :samples N :stroke {:size 1.5}}]
                                [:line (map vector xtest mu) {:color :black :stroke {:size 2}}]

                                [:scatter (map vector xs ys) {:size 10}])
                      (b/add-axes :bottom)
                      (b/add-axes :left)
                      (b/add-label :bottom "Gaussian Process - prediction sampled"))
            (save "results/examples/gp-predict.jpg")
            (show))))

(let [N 100
      xs [-4 1 2]
      ys [-5 1 2]
      xtest (map #(m/norm % 0 (dec N) -5.0 5.0) (range N))
      gp (gp/gaussian-process xs ys {:kernel (k/kernel :gaussian 0.8) :noise 0.0001})
      [mu stddev] (gp/posterior-samples gp xtest true)
      s95 (map #(* 1.96 %) stddev)]
  (-> (xy-chart {:width 800 :height 600}
                (b/series [:grid]
                          [:ci [(map vector xtest (map - mu s95)) (map vector xtest (map + mu s95))] {:color (c/color :lightblue 180)}]                          
                          [:line (map vector xtest mu) {:color :black :stroke {:size 2}}]
                          [:scatter (map vector xs ys) {:size 10}])
                (b/add-axes :bottom)
                (b/add-axes :left)
                (b/add-label :bottom "Gaussian Process - posterior samples"))
      ;; (save "results/examples/gp-posterior.jpg")
      (show)))


;; TO REMOVE
;;

(time (let [k (kk/kernel->rbf (kk/kernel :scalar-functions #(m/log1p (m/abs (first %)))))
            d [-5 -2 -1 -0.2 0 4 4.3 4.4 4.5 5.0]
            r (map sinf d)
            i (in/rbf (kk/smile-rbf k) d r)]
        (-> (xy-chart {:width 400 :height 400}
                      (b/series [:grid]
                                [:function sinf {:domain [-5 5] :samples 200 :color :gray}]
                                [:function i {:domain [-5 5] :samples 200}]
                                [:scatter (map vector d r)])
                      (b/add-axes :bottom)
                      (b/add-axes :left))
            (show))))

(let [k (kk/rbf :wendland-10)]
  (-> (xy-chart {:width 400 :height 400}
                (b/series [:grid] [:function k {:domain [-1.1 1.1] :samples 200}])
                (b/add-axes :bottom)
                (b/add-axes :left))
      (show)))

#_(let [w 600 h 600]
    (c2d/with-canvas [cvs (c2d/canvas w h)]
      (c2d/set-background cvs :black)

      
      (let [wnd (c2d/show-window cvs "ring")]


        (c2d/set-color cvs :white )
        (c2d/rect cvs 300 200 300 200 )
        (c2d/set-color cvs :black 30)
        (c2d/rect cvs 0 0 w h false)
        (c2d/rect cvs 0 0 w h false)
        ;; (c2d/rect cvs 0 0 w h false)
        ;; (c2d/rect cvs 0 0 w h false)
        ;; (c2d/rect cvs 0 0 w h false)
        ;; (c2d/rect cvs 0 0 w h false)
        (c2d/rect cvs 0 0 w h false)
        (c2d/rect cvs 0 0 w h false)
        (c2d/rect cvs 0 0 w h false)

        )))

#_(-> (xy-chart {:width 500 :height 300}
                (b/series [:grid] [:line (map vector (range 50) (iterate (fn [x] (* x 0.85)) 1)) {:stroke {:size 3} :samples 300}])
                (b/update-scale :x :fmt int)
                (b/add-label :bottom "Layers")
                (b/add-label :left "Brightness (luma)")
                (b/add-axes :bottom)
                (b/add-axes :left))
      (save "alpha.jpg"))


(let [draw (fn [canvas _ _ _]
             (when (rnd/brand 0.9)
               (let [x (rnd/drand (c2d/width canvas))
                     y (rnd/drand (c2d/height canvas))]
                 (c2d/filled-with-stroke canvas (c/color (rnd/drand 255) (rnd/drand 255) (rnd/drand 255)) :white c2d/ellipse x y 40 40)))
             (-> canvas
                 (c2d/set-color :black 10)
                 (c2d/rect 0 0 400 400)
                 (p/set-canvas-pixels! (p/filter-channels (p/box-blur 15) (p/to-pixels canvas)))
                 ))]

  (c2d/show-window {:canvas (c2d/with-canvas-> (c2d/canvas 400 400)
                              (c2d/set-background :black))
                    :draw-fn draw}))

(c2d/show-window {:canvas (let [c (c2d/with-canvas-> (c2d/canvas 100 100)
                                    (c2d/set-background :black)
                                    (c2d/set-color :red)
                                    (c2d/rect 0 0 40 40))
                                res (c2d/with-canvas [c c]
                                      (reduce (fn [i _]
                                                (-> i
                                                    (c2d/set-color :black 10)
                                                    (c2d/rect 0 0 100 100)
                                                    (p/set-canvas-pixels! (p/filter-channels p/horizontal-blur-5 (p/to-pixels i))))) c (range 1500)))]
                            (print (take 10 (p/to-pixels res)))
                            res)})
