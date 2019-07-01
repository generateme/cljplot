(ns bayesian-optimisation
  (:require [cljplot.render :as r]
            [cljplot.build :as b]
            [cljplot.common :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as rnd]
            [cljplot.core :refer :all]
            [fastmath.optimization :as opt]
            [fastmath.kernel :as k]
            [fastmath.regression :as reg]
            [fastmath.classification :as cl]
            [clojure2d.color :as c]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)
(m/unuse-primitive-operators)

;; first regression

(def domain [-3 3])

(def xs [[0] [1] [-2] [-2.001]]) ;; 4 points as vectors
(def ys [-2 3 0.5 -0.6]) ;; 4 values

(defn draw-gp
  [xs ys title legend-name labels gps & [h w]]
  (let [pal (c/palette-presets :category10)
        leg (map #(vector :line %1 {:color %2}) labels pal)]
    (-> (xy-chart {:width (or w 700) :height (or h 300)}
                  (-> (b/series [:grid]
                                [:scatter (map vector (map first xs) ys) {:size 8}])
                      (b/add-multi :function gps {:samples 400
                                                  :domain domain} {:color pal}))
                  (b/add-axes :bottom)
                  (b/add-axes :left)
                  (b/add-legend legend-name leg)
                  (b/add-label :top title)))))

(def kernels [:gaussian :cauchy :anova :linear :inverse-multiquadratic
              :mattern-12 :mattern-52 :periodic :exponential])

(show (draw-gp xs ys "GP regression with different lambda"
               "Kernel" kernels 
               (map vector kernels 
                    (map #(reg/gaussian-process {:kernel (k/kernel %)} xs ys) kernels))))

(let [xs [[0] [1] [-2] [-2.001]]
      y [1 -1 0.5 -0.6]]
  (println ((reg/gaussian-process {:lambda 0.00005} xs y) 0))
  (show (draw-gp xs y "Gaussian processes with different noise"
                 "Lambda" [0.0005 0.1 2]
                 {:l1 (reg/gaussian-process {:lambda 0.00005} xs y)
                  :l2 (reg/gaussian-process {:lambda 0.1} xs y)
                  :l3 (reg/gaussian-process {:lambda 2} xs y)})))

(def gps {:l1 (reg/gaussian-process {:lambda 0.00005} xs ys)
          :l2 (reg/gaussian-process {:lambda 0.1} xs ys)
          :l3 (reg/gaussian-process {:lambda 2} xs ys)})

(def scatter-data (map vector (map first xs) ys))

(show (xy-chart {:width 700 :height 600}
                (-> (b/lattice :function gps {:domain domain} {:shape [-1 1] :grid true})
                    (b/add-series (b/lattice :scatter (zipmap (keys gps) (repeat scatter-data)) {:size 8} {:label name :shape [-1 1]})))
                (b/add-axes :bottom)
                (b/add-axes :left)
                (b/add-label :top "Various parametrization")))

(defn gen-gps
  [lambdas kernels kernel-params]
  (map (fn [l k p]
         (let [n (str "Kernel: " k "(" p "), lambda=" l)
               kernel (k/kernel k p)]
           [n (reg/gaussian-process {:kernel kernel :lambda l} xs ys)])) (cycle lambdas) (cycle kernels) kernel-params))

(gen-gps [0.1 0.2] [:gaussian] [0.1 0.9])

(defn draw-gp-lattice
  [title gps]
  (let [gps (into {} gps)]
    (xy-chart {:width 700 :height 600}
              (-> (b/lattice :function gps {:domain domain} {:shape [-1 1] :grid true})
                  (b/add-series (b/lattice :scatter (zipmap (keys gps) (repeat scatter-data)) {:size 8} {:label name :shape [-1 1]})))
              (b/add-axes :bottom)
              (b/add-axes :left)
              (b/add-label :top title))))

(show (draw-gp-lattice "Various gaussian kernels" (gen-gps [0.001] [:gaussian] [0.1 1 4])))

(seq (m/seq->double-array 3))

(defn toy-fn
  [[x]]
  (+ (rnd/grand) (m/exp x) (* -5 (m/cos (* 3 x)))))

(show (xy-chart {:width 700 :height 300}
                (b/series [:grid] [:function (comp toy-fn vector) {:domain [-3 3]}])
                (b/add-axes :bottom)
                (b/add-axes :left)))

(def xs (repeatedly 50 #(vector (apply rnd/drand domain))))
(def ys (map toy-fn xs))

(show (draw-gp xs ys "Gaussian processes with different noise"
               "Lambda"
               [0.5]
               {:l1 (reg/gaussian-process {:lambda 10} xs ys)
                :l2 (reg/gaussian-process {:lambda 0.0001} xs ys)} 300))

(defn draw-prior
  ([gp] (draw-prior gp 20))
  ([gp cnt]
   (let [xs (range -3 3.1 0.1)
         priors (map #(vector % (map vector xs (reg/prior-samples gp xs))) (range cnt))]
     (xy-chart {:width 700 :height 300}
               (-> (b/series [:grid])
                   (b/add-multi :line priors {} {:color (cycle (c/palette-presets :category20c))}))
               (b/add-axes :bottom)
               (b/add-axes :left)
               (b/add-label :top "Priors")))))

(def xs [-4 -1 2])
(def ys [-5 -1 2])

(defn draw-stddev
  [gp]
  (let [xxs (range -5 5.1 0.2)
        pairs (reg/predict-all gp xxs true)
        mu (map first pairs)
        stddev (map second pairs)
        s95 (map (partial * 1.96) stddev)
        s50 (map (partial * 0.67) stddev)]
    (xy-chart {:width 700 :height 300}
              (b/series [:grid]
                        [:ci [(map vector xxs (map - mu s95)) (map vector xxs (map + mu s95))] {:color (c/color :lightblue 180)}]
                        [:ci [(map vector xxs (map - mu s50)) (map vector xxs (map + mu s50))] {:color (c/color :lightblue)}]
                        [:line (map vector xxs mu) {:color :black :stroke {:size 2 :dash [5 2]}}]
                        [:scatter (map vector xs ys) {:size 8}])
              (b/add-axes :bottom)
              (b/add-axes :left)
              (b/add-label :top "Confidence intervals"))))

(show (draw-stddev (reg/gaussian-process+ xs ys)))

(keys (methods k/kernel))
;; => (:laplacian :hyperbolic-secant :histogram :chi-square-cpd :variance-function :periodic :mattern-52 :gaussian :multiquadratic :chi-square-pd :scalar-functions :mattern-12 :spherical :power :hyperbolic-tangent :spline :generalized-t-student :cauchy :pearson :dirichlet :exponential :bessel :polynomial :linear :circular :hellinger :wave :log :anova :mattern-32 :rational-quadratic :generalized-histogram :inverse-multiquadratic :thin-plate)


;; https://github.com/fmfn/BayesianOptimization/blob/master/examples/visualization.ipynb

(defn target
  "1d target function for optimization."
  ^double [^double x]
  (+ (m/exp (- (m/sq (- x 2.0))))
     (m/exp (- (* 0.1 (m/sq (- x 6.0)))))
     (/ (inc (* x x)))))

(defn target
  ^double [^double x]
  (- (+ (* x (m/sin x))
        (* x (m/cos (+ x x))))))

(defn target
  ^double [^double x]
  (- (/ (+ (m/sq x) (* -5.0 x) 6.0)
        (inc (m/sq x)))))

(defn target
  ^double [^double x]
  (* (+ x (m/sin x))
     (m/exp (- (m/sq x)))))

(defn target
  ^double [^double x]
  (- (+ 10.0 (- (* x x) (* 10.0 (m/cos (* m/TWO_PI x)))))))

(defn black-box-function2
  ^double [^double x ^double y]
  (* (m/sinc (+ 2.0 x y))
     (m/sin (+ x x))
     (m/cos (+ y y))
     (m/sin (* 2.0 x y))))

(defn rastrigin
  ^double [^double x ^double y]
  (- (+ 20 (m/sq x) (* -10 (m/cos (* m/TWO_PI x)))
        (m/sq y) (* -10 (m/cos (* m/TWO_PI y))))))

(def bounds2 [[-1.0 0.5] [-2 -0.5]])

(def bo2 (opt/bayesian-optimization black-box-function2 {:bounds bounds2}))

(keys (first bo2))
;; => (:x :y :util-fn :gp :xs :ys :util-best)

(let [{:keys [x y gp util-fn]} (nth bo2 5)
      [m s] (gp [0 0] true)
      d (/ (- m y) s)]
  (println [m s y 0])
  (rnd/cdf rnd/default-normal d))

(let [id 15
      pal (reverse (:rdylbu-9 c/palette-presets))
      {:keys [util-fn gp xs]} (nth bo2 id)
      b {:x (first bounds2)
         :y (second bounds2)
         :palette pal
         :contours 30
         :gradient (c/gradient pal)}]
  (show (xy-chart {:width 700 :height 700}
                  (b/series [:contour-2d black-box-function2 (assoc b :position [0 1])]
                            [:scatter xs {:size 10 :color (c/darken (first pal))
                                          :margins {:x [0 0] :y [0 0]}
                                          :position [0 1]
                                          :label "2d function with guessed points."}]
                            [:function-2d util-fn (assoc b :position [0 0] :label "Utility function")]
                            [:contour-2d (fn [x y] (gp [x y])) (assoc b :position [1 1] :label "Gaussian processes - mean (interpolation)")]
                            [:contour-2d (fn [x y] (second (gp [x y] true))) (assoc b :position [1 0] :label "Gaussian processes - std dev")])
                  (b/add-axes :bottom)
                  (b/add-axes :left))))

(def bounds [-5.12 5.12])


(-> (xy-chart {:width 600 :height 600}
              (b/series [:grid]
                        [:function target {:domain bounds :samples 500}])
              (b/add-axes :bottom)
              (b/add-axes :left))
    (show))
;; => 
(def optimizer (opt/bayesian-optimization target {:kernel (k/kernel :mattern-52)
                                                  :bounds [bounds]
                                                  :utility-function-type :poi
                                                  :utility-param 1
                                                  :jitter 0
                                                  :kernel-scale 0.0001
                                                  :init-points 5}))

(defn draw-bo
  ([opt] (draw-bo opt 0))
  ([opt idx]
   (let [{:keys [x y util-fn gp xs ys util-best]} (nth opt idx)
         pairs (map vector (map first xs) ys)
         [x1 x2] bounds
         xtest (map #(m/norm % 0 199 x1 x2) (range 200))
         ms-pairs (reg/predict-all gp xtest true)
         mu (map first ms-pairs)
         stddev (map second ms-pairs)
         s95 (map (partial fast* 1.96) stddev)
         s50 (map (partial fast* 0.67) stddev)]
     (-> (xy-chart {:width 800 :height 500}
                   (b/series [:grid]
                             [:ci [(map vector xtest (map fast- mu s95)) (map vector xtest (map fast+ mu s95))] {:color (c/color :lightblue 120)}]
                             [:ci [(map vector xtest (map fast- mu s50)) (map vector xtest (map fast+ mu s50))] {:color (c/color :lightblue)}]
                             [:function target {:stroke {:size 2} :domain bounds :samples 400}]
                             [:line (map vector xtest mu) {:color :darkblue :stroke {:dash [20 3]}}]
                             [:vline (first util-best) {:color :black :size 2 :dash [10 5]}]
                             [:scatter pairs {:size 8 :color :darkcyan}]
                             [:scatter [[(first x) y]] {:color :maroon :size 10}])
                   (b/add-side :top 100 (b/series [:grid nil {:y nil}]
                                                  [:function util-fn {:domain bounds :samples 400}]
                                                  [:vline (first util-best) {:color :black :size 2 :dash [10 5]}]))
                   (b/add-axes :bottom)
                   (b/add-axes :left))
         (show)))))

(draw-bo optimizer 20)

(-> (xy-chart {:width 600 :height 600}
              (b/series [:grid]
                        [:function (:util-fn (nth optimizer 15)) {:domain [-2 10]}])
              (b/add-axes :bottom)
              (b/add-axes :left))
    (show))


(keys (nth optimizer 0));; => (:x :y :util-fn :gp :xs :ys)


;;

(def dataset (with-open [data (io/reader "data/sonar.csv")]
               (mapv #(-> %
                          (str/trim)
                          (str/split #",")
                          (->> (map read-string))) (line-seq data))))

(keyword (last (first dataset)))

(def xs (map butlast dataset))
(def ys (map (comp keyword last) dataset))

(defn ada-boost-params
  ^double [^double trees ^double nodes]
  (let [ada-boost (cl/ada-boost {:number-of-trees (int trees)
                                 :max-nodes (int nodes)} xs ys)]
    (:accuracy (cl/cv ada-boost))))

(defn dt-params
  ^double [^double trees ^double nodes]
  (let [dt (cl/decision-tree {:split-rule :classification-error
                              :number-of-trees (int trees)
                              :max-nodes (int nodes)} xs ys)]
    (:accuracy (cl/cv dt))))


(defn svm-params
  ^double [^double cp ^double cn]
  (let [cp (m/pow 10 cp)
        cn (m/pow 10 cn)
        svm (cl/svm {:kernel (k/kernel :gaussian)
                     :c-or-cp cp
                     :cn cn} xs ys)]
    (m/log (:accuracy (cl/cv svm)))))

(defn nn-params
  [l1 l2 l3 lr mm dc]
  (let [l1 (int (* ^double l1 100))
        l2 (int (* ^double l2 100))
        l3 (int (* ^double l3 100))
        nn (cl/neural-net {:layers [l1 l2 l3]
                           :learning-rate lr
                           :momentum mm
                           :weight-decay (* 0.1 ^double dc)} xs ys)]
    (:accuracy (cl/cv nn))))

(ada-boost-params 1 200)

(m/exp (svm-params 1 1))

(nn-params 0.2 0.2 0.01 0.1 0 0)

(dt-params 100 200)

(def dtbounds [[1 1000]
               [2 1000]])

(def nnbounds [[0.01 1.0]
               [0.01 1.0]
               [0.01 1.0]
               [0.01 0.99]
               [0.0 0.99]
               [0.0 0.99]])

(def bo (opt/bayesian-optimization dt-params
                                   {:init-points 10
                                    :kernel (k/kernel :mattern-52 200)
                                    :utility-function-type :ucb
                                    :utility-param 0.3
                                    :noise 0.5
                                    :bounds dtbounds}))

(def palette (reverse (:rdylbu-9 c/palette-presets)))
(def gradient (c/gradient palette))

(defn draw-2d-2
  [bounds bayesian-optimizer iteration]
  (let [{:keys [util-fn gp xs]} (nth bayesian-optimizer iteration)
        cfg {:x (first bounds)
             :y (second bounds)
             :palette palette
             :contours 30}]
    (xy-chart {:width 700 :height 300}
              (b/series [:contour-2d (fn [x y] (gp [x y])) cfg]
                        [:contour-2d (fn [x y] (second (gp [x y] true))) 
                         (assoc cfg :position [1 0]
                                :label "Gaussian processes - std dev")]
                        [:scatter xs {:size 10 
                                      :color (c/darken (first palette))
                                      :margins {:x [0 0] :y [0 0]}
                                      :position [0 0]
                                      :label "Gaussian processes - mean (interpolation)"}])
              (b/add-axes :bottom)
              (b/add-axes :left))))

(show (draw-2d-2 dtbounds bo 50))

[[1 (count dataset)]
 [2 (count dataset)]]

(select-keys (nth bo 30) [:x :y])

#_(m/exp (:y (nth ada-boost-bo 30)))
