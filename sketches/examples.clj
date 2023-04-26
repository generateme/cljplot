(ns cljplot.sketches.examples
  (:require [cljplot.render :as r]
            [cljplot.build :as b]
            [cljplot.common :as common]
            [fastmath.stats :as stats]
            [clojure2d.color :as c]
            [cljplot.scale :as s]
            [fastmath.core :as m]
            [fastmath.random :as rnd]
            [cljplot.core :refer [save xy-chart show]]
            [java-time :as dt]
            [clojure2d.core :as c2d]
            [clojure2d.pixels :as p]
            [fastmath.complex :as cx]
            [fastmath.fields :as f]
            [fastmath.vector :as v]
            [fastmath.gp :as gp]
            [fastmath.kernel :as k]))

;; logo

(rnd/set-seed! rnd/default-rng 2)

(def logo (p/to-pixels (c2d/with-oriented-canvas-> :bottom-left+ (c2d/canvas 200 100)
                       (c2d/set-font "Raleway Thin")
                       (c2d/set-background :black)
                       (c2d/set-color :white)
                       (c2d/set-font-attributes 60)
                       (c2d/text "cljplot" 20 70))))

(let [gradient (c/gradient (c/palette :two-heads-filonov))
      side-conf {:color (c/set-alpha (gradient 0.3) 200) :area? true :margins {:x [0.02 0.02]}}
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
      #_(save "results/examples/logo.jpg")
      (show)))

;; single line charts

(let [blue (last (c/palette :rdylbu-9))
      red (first (c/palette :rdylbu-9))
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
  (-> (b/series [:cloud data {:kernel :gaussian :logarithmic? false :gradient (c/gradient (c/palette :tornyai))}]
                [:grid])
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (r/render-lattice {:width 800 :height 555})
      (save "results/examples/point-cloud.jpg")
      (show)))

(let [data (repeatedly 10000 #(rnd/randval [(rnd/grand) (rnd/grand)]
                                           [(rnd/grand -10 1) (rnd/grand -10 1)]))]
  (-> (b/series [:grid] [:binned-heatmap data {:grid :flat-hex :alpha-factor 0 :size 20 :gradient (c/gradient (c/palette :prl-2))}])
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

(defn fn2d
  [^double x ^double y]
  (* (- (+ (* x x)
           (* y y)))
     (m/sin x)
     (m/sin y)
     (m/sin (* x y))))

(-> (b/series [:function-2d fn2d {:x [m/-PI m/PI] :y [m/-PI m/PI]}])
    (b/preprocess-series)
    (b/add-axes :bottom)
    (b/add-axes :left)
    (b/add-label :bottom "2d function")
    (r/render-lattice {:width 600 :height 600})
    (save "results/examples/function2d.jpg")
    (show)) 

(-> (b/series [:contour-2d fn2d {:x [m/-PI m/PI] :y [m/-PI m/PI]}])
    (b/preprocess-series)
    (b/add-axes :bottom)
    (b/add-axes :left)
    (b/add-label :bottom "2d function")
    (r/render-lattice {:width 600 :height 600})
    (save "results/examples/contour2d.jpg")
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

(def faithful (common/read-json "data/faithful.json"))

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
(def sunspots (map :x (common/read-json "data/sunspot_year.json")))

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
      m (keys (methods k/kernel-density))
      kdes (zipmap m (map #(k/kernel-density % data) m))]
  (-> (xy-chart {:width 600 :height 600}
                (-> (b/series [:grid] )
                    (b/add-multi :function kdes
                                 {:points 600 :domain [1.0 6.0] :stroke {:size 1.5}} {:color (cycle (map #(c/set-alpha % 200) (c/palette :category10)))}))
                (b/add-axes :bottom)
                (b/add-axes :left)
                (b/add-label :bottom "Various kernel densities"))
      (save "results/examples/kernel-densities.jpg")
      (show)))

;;


(defn sinf [v] (+ (rnd/grand 0.0005) (+ 10 (m/sin (- (* 0.7 v))))))

(time (let [N 100
            n 10
            xs (repeatedly n #(rnd/drand -5 5))
            ys (map sinf xs)
            gp (gp/gaussian-process xs ys {:normalize? true :kernel
                                           (k/kernel :gaussian 0.8)
                                           :noise 0.0005})
            xtest (map #(m/norm % 0 (dec N) -5.0 5.0) (range N))
            pairs (gp/predict-all gp xtest true)
            mu (map first pairs)
            stddev (map second pairs)
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

(let [N 35
      xs [-4 1 2]
      ys [-5 1 2]
      xtest (sort (map #(m/norm % 0 (dec N) -5.0 5.0) (range N)))
      gp (gp/gaussian-process xs ys {:kernel (k/kernel :gaussian 0.8) :noise 0.005})
      pairs (gp/posterior-samples gp xtest true)
      mu (map first pairs)
      stddev (map second pairs)
      s95 (map #(* 1.96 %) stddev)]
  (-> (xy-chart {:width 800 :height 600}
                (b/series [:grid]
                          [:ci [(map vector xtest (map - mu s95)) (map vector xtest (map + mu s95))] {:color (c/color :lightblue 180) :smooth? true}]                          
                          [:line (map vector xtest mu) {:color :black :stroke {:size 2} :smooth? true}]
                          [:scatter (map vector xs ys) {:size 10}])
                (b/add-axes :bottom)
                (b/add-axes :left)
                (b/add-label :bottom "Gaussian Process - posterior sample"))
      (save "results/examples/gp-posterior.jpg")
      (show)))

(let [posterior-cnt 30
      N 30
      xs [-4 1 2]
      ys [-5 1 2]
      xtest (map #(m/norm % 0 (dec N) -5.0 5.0) (range N))
      gps (repeatedly posterior-cnt #(gp/gaussian-process xs ys {:kernel (k/kernel :periodic 0.5 8.0) :noise 0.0001}))
      pairs (map #(gp/posterior-samples % xtest true) gps)
      lines (map #(vector :line (map vector xtest (map first %)) {:color (c/color :black 100) :smooth? true}) pairs)]
  (-> (xy-chart {:width 800 :height 600}
                (-> (b/series [:grid])
                    (b/add-series lines)
                    (b/add-serie [:scatter (map vector xs ys) {:size 10}]))
                
                (b/add-axes :bottom)
                (b/add-axes :left)
                (b/add-label :bottom "Gaussian Process - posterior samples (20)"))
      (save "results/examples/gp-posteriors-20.jpg")
      (show)))

(let [prior-cnt 20
      N 35
      xs [-4 1 2]
      ys [-5 1 2]
      xtest (map #(m/norm % 0 (dec N) -5.0 5.0) (range N))
      gps (repeatedly prior-cnt #(gp/gaussian-process xs ys {:kernel (k/kernel :gaussian) :noise 0.0000001}))
      pairs (map #(gp/prior-samples % xtest) gps)
      lines (map #(vector :line (map vector xtest %) {:color (c/color :black 100) :smooth? true}) pairs)]
  (-> (xy-chart {:width 800 :height 600}
                (-> (b/series [:grid])
                    (b/add-series lines))
                (b/add-axes :bottom)
                (b/add-axes :left)
                (b/add-label :bottom "Gaussian Process - prior samples (20)"))
      (save "results/examples/gp-priors-20.jpg")
      (show)))


;;;;;

(-> (b/series
     [:grid]
     [:frequencies [[1 2 2 3 1 12 2] [3 4 5 4 5 4 3 2 3 4 11 12]] {:padding-out 0.5 :sort? true :range? true :pmf? false :type :lollies}])
    (b/preprocess-series)
    (b/add-axes :bottom)
    (b/add-axes :left)
    (r/render-lattice)
    (show))

(def hdata  (repeatedly 1300 rnd/grand))
(def hdata2 (repeatedly 100 rnd/grand))

(-> (b/series
     [:grid]
     [:histogram hdata {:cumulative? false :density? true :bins 130 :type :lollipops}]
     [:density hdata {:color (c/color :red 120) :area? true :kernel-bandwidth 0.04}])
    (b/preprocess-series)
    (b/add-axes :bottom)
    (b/add-axes :left)
    (r/render-lattice)
    (show))

;; BUBBLE example

(defn parse-line
  [defs line]
  (mapv #((or %1 read-line) %2) defs line))

(def sweather (->> (common/read-csv "data/seattle-weather.csv")
                 (map (partial parse-line [(partial dt/local-date "yyyy/MM/dd")
                                           read-string read-string read-string read-string keyword]))
                 (map rest)))

;; Seattle weather dataset
;; t1 - minimal temperature. x-axis
;; t2 - maximal temperature, y-axis
;; w - wind, color gradient
;; p - precipitation, size

(let [data (map (fn [[p t1 t2 w]] [t2 t1 w p]) sweather) ;; reorganize data
      [min-wind max-wind] (stats/extent (map #(nth % 2) data)) ;; find out wind extent
      [min-prec max-prec] (stats/extent (map #(nth % 3) data)) ;; find out precipitation extent
      wind-map (s/linear [min-wind max-wind]) ;; create linear scale for wind (just lerp)
      grad (comp #(c/set-alpha % 200)
                 c/darken
                 (c/gradient (c/palette :gnbu-9))
                 wind-map)] ;; create gradient from green-blue palette, darkened
  (-> (xy-chart {:width 600 :height 600}
                (b/series
                 [:grid]
                 [:scatter data {:color (fn [[_ _ w] _] (grad w)) ;; callback function which returns color from gradient based on wind
                                 :size (fn [[_ _ _ p] _] (m/norm p min-prec max-prec 2 50)) ;; callback for size based on precipitation
                                 }])
                (b/add-axes :bottom)
                (b/add-axes :left)
                (b/add-label :bottom "Minimal temperature")
                (b/add-label :left "Maximal temperature")
                (b/add-label :top "Seattle weather"))
      (save "results/bubble.jpg")
      (show)))

;; heatmap matrix

(let [data (for [x (range 40)
                 y (range 40)]
             [[x y] (rnd/drand x y)])]
  (-> (xy-chart {:width 600 :height 600}
                (b/series [:heatmap data ])
                (b/add-axes :bottom)
                (b/add-axes :left)
                (b/add-label :left "Y")
                (b/add-label :bottom "X"))
      (save "results/examples/heatmap-matrix.jpg")
      (show)))

(let [data (for [x (range 15)
                 y (range 15)]
             [[x y] (rnd/drand x y)])]
  (-> (xy-chart {:width 600 :height 600}
                (b/series [:heatmap data {:gradient (c/gradient :yellow-red)
                                          :annotate? true
                                          :annotate-fmt "%.1f"}])
                (b/add-axes :bottom)
                (b/add-axes :left)
                (b/add-label :left "Y")
                (b/add-label :bottom "X"))
      (save "results/examples/heatmap-matrix-ann.jpg")
      (show)))

;; kernels

(def kernels [:gaussian :periodic :cauchy :linear
              :mattern-12 :mattern-52])

(let [fk (fn [f] #(f [0] [%]))
      make-data #(map (fn [k] [(str k)
                               (fk (k/kernel k %))]) kernels)
      cfg {:domain [-3 3] :samples 200 :stroke {:size 2}}]
  (-> (xy-chart {:width 700 :height 500}
                (-> (b/lattice :function (make-data 1) cfg {:label name :grid true})
                    (b/add-series (b/lattice :function (make-data 0.5) (assoc cfg :color (c/color 215 50 40)) {:label name :grid true})))
                (b/add-label :top "Various kernels around 0")
                (b/add-axes :bottom)
                (b/add-axes :left)
                (b/add-axes :right))
      (show)))

(let [r (range -1 1 0.025)
      data (map (fn [k] [(str k)
                        (let [kernel (k/kernel k)]
                          (for [x r y r] [[x y] (kernel [x] [y])]))]) kernels)]
  (-> (xy-chart {:width 700 :height 500}
                (-> (b/lattice :heatmap data {} {:label name :grid true}))
                (b/add-label :top "Covariance matrices"))
      (show)))


(let [k1 (k/kernel :gaussian)
      k2 (k/kernel :periodic 1.0 2)
      k3 (k/kernel :thin-plate 2)
      r (range -3 3 0.25)
      data #(for [x r
                  y r]
              [[x y] (% [x] [y])])]
  (-> (xy-chart {:width 900 :height 325}
                (b/series [:heatmap (data k1) {:position [0 0] :label "gaussian"}]
                          [:heatmap (data k2) {:position [1 0] :label "periodic"}]
                          [:heatmap (data k3) {:position [2 0] :label "thin-plate"}])
                (b/add-label :top "Covariance matrices for different kernels")
                (b/update-scales :x :ticks 8)
                (b/update-scales :y :ticks 8)
                (b/add-axes :bottom)
                (b/add-axes :left))
      (show)))

;; hline, vline, abline

(-> (xy-chart {:width 900 :height 300}
              (b/series [:hline 0 {:color :black}]
                        [:hline 1]
                        [:hline -1]
                        [:vline m/HALF_PI {:dash [10 10] :size 5 :cap :round}]
                        [:vline (* 0.75 m/TWO_PI) {:dash [10 10]
                                                   :dash-phase 5
                                                   :size 5
                                                   :cap :round
                                                   :color (c/set-alpha :maroon 100)}]
                        [:abline [(/ (* 0.75 m/TWO_PI)) -1] {:dash [1 2 5 2]
                                                             :size 3}]
                        [:function #(m/sin %) {:samples 800 :domain [0 m/TWO_PI]}]) 
              (b/add-axes :bottom)
              (b/add-axes :left))
    (save "results/examples/lines.jpg")
    (show))

;;=> (:round :butt :square) cap
;;=> (:bevel :miter :round) join

(def boundary-left -200.0)
(def boundary-right 200.0)
(def width (- boundary-right boundary-left))

(defn correct-path
  [path]
  (first (reduce (fn [[new-path shift-x shift-y] [[curr-x curr-y] [next-x next-y]]]
                   (let [s-curr-x (+ shift-x curr-x)
                         s-curr-y (+ shift-y curr-y)
                         s-next-x (+ shift-x next-x)
                         s-next-y (+ shift-y next-y)
                         new-shift-x (cond
                                       (< s-next-x boundary-left) (+ shift-x width)
                                       (> s-next-x boundary-right) (- shift-x width)
                                       :else shift-x)
                         new-shift-y (cond
                                       (< s-next-y boundary-left) (+ shift-y width)
                                       (> s-next-y boundary-right) (- shift-y width)
                                       :else shift-y)]
                     [(if (and (== new-shift-x shift-x)
                               (== new-shift-y shift-y))
                        (conj new-path [s-curr-x s-curr-y])
                        (conj new-path
                              [s-curr-x s-curr-y] [s-next-x s-next-y]
                              [##NaN ##NaN] ;; new chunk separator
                              [(+ curr-x new-shift-x) (+ curr-y new-shift-y)]))
                      new-shift-x
                      new-shift-y]))
                 [[] 0.0 0.0] (partition 2 1 path))))

(defn walk-step
  [[x y]]
  [(+ x 2.0 (rnd/grand 2.0))
   (+ y 0.5 (rnd/grand 2.0))])

(def some-walk (iterate walk-step [0.0 0.0]))

(-> (b/series [:grid] [:line (correct-path (take 3000 some-walk))
                       {:color [0 50 255 150] :margins nil}])
    (b/preprocess-series)
    (b/update-scale :x :domain [-200 200])
    (b/update-scale :y :domain [-200 200])
    (b/add-axes :bottom)
    (b/add-axes :left)
    (r/render-lattice {:width 800 :height 800})
    (save "results/examples/random-walk-wrapped.jpg")
    (show))
