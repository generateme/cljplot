(ns cljplot.sketches.vega
  (:require [cljplot.render :as r]
            [cljplot.build :as b]
            [cljplot.common :refer :all]
            [fastmath.interpolation :as in]
            [fastmath.stats :as stats]
            [clojure2d.color :as c]
            [cljplot.scale :as s]
            [fastmath.core :as m]
            [cljplot.core :refer :all]
            [java-time :as dt]
            [fastmath.random :as rnd]))

(rnd/set-seed! rnd/default-rng 143)

;; global fns

(defn parse-line
  [defs line]
  (mapv #((or %1 read-line) %2) defs line))

;; load all data

(do
  (def movies (read-json "data/movies.json"))
  (def population (read-json "data/population.json"))

  (def population-male-female (->> (filter #(= (:year %) 2000) population)
                                 (group-by :age)
                                 (map-kv (fn [v]
                                           (let [m (group-by :sex v)]
                                             [(reduce + (map :people (m 2)))
                                              (reduce + (map :people (m 1)))])))
                                 (into (sorted-map))))

  (def seattle-weather (map (partial parse-line [(partial dt/local-date "yyyy/MM/dd")
                                                 read-string read-string read-string read-string keyword])
                            (read-csv "data/seattle-weather.csv")))

  (def barley (read-json "data/barley.json"))

  (def barley-variety-yield (->> barley
                               (group-by :variety)
                               (map-kv #(map :yield %))
                               (sort-by first)
                               (reverse)))
  
  (def cars (read-json "data/cars.json"))

  (def gapminder-health (map #(map read-string %)
                             (read-csv "data/gapminder-health-income.csv")))

  (def disasters (map (partial parse-line [identity read-string read-string])
                      (read-csv "data/disasters.csv")))

  (def dt-stocks-fmt (-> (java.time.format.DateTimeFormatterBuilder.)
                        (.appendPattern "MMM d yyyy")
                        (.toFormatter (java.util.Locale/ENGLISH))))

  (def stocks (map (partial parse-line [keyword (partial dt/local-date dt-stocks-fmt) read-string])
                   (read-csv "data/stocks.csv")))

  (def driving (read-json "data/driving.json"))

  (def co2-concentration (map (partial parse-line [(partial dt/local-date "yyyy-MM-dd") read-string])
                              (read-csv "data/co2-concentration.csv")))

  (def unemployment (read-json "data/unemployment-across-industries.json"))

  (def unemployment-area (->> unemployment
                            (group-by :series)
                            (map-kv #(map (fn [{:keys [year month count]}]
                                            [(dt/local-date year month) count]) %))
                            (sort-by first (comp - compare))))
  
  (def blue (last (:rdylbu-9 c/palette-presets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BARS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://vega.github.io/vega-lite/examples/bar.html

(let [data (sorted-map :A 28 :B 55 :C 43 :D 91 :E 81 :F 53 :G 19 :H 87 :I 52)]
  
  (-> (b/series
       [:grid nil {:x nil}]
       [:stack-vertical [:bar data {:padding-out 0.1}]])
      (b/preprocess-series)
      (b/update-scale :x :fmt name)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "a")
      (b/add-label :left "b")
      (r/render-lattice {:width 400 :height 400})
      (save "results/vega/bar.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/histogram.html

(let [data (filter (complement nil?) (map :IMDB_Rating movies))]
  
  (-> (b/series
       [:grid nil {:x nil}]
       [:histogram data {:percents? false :bins 8 :padding-out 0.1}])
      (b/preprocess-series)
      (b/update-scale :y :fmt int)
      (b/add-axes :left)
      (b/add-axes :bottom)
      (b/add-label :bottom "IMDB_Rating (binned)")
      (b/add-label :left "Count of Records")
      (r/render-lattice {:width 400 :height 400})
      (save "results/vega/histogram.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/bar_aggregate.html

(let [data (->> (filter #(= (:year %) 2000) population)
                (map #(vector (:age %) (:people %)))
                (group-by first)
                (map-kv (fn [v] (reduce + (map second v))))
                (into (sorted-map-by >)))]
  
  (-> (b/series
       [:grid nil {:y nil}]
       [:stack-horizontal [:bar data {:padding-out 0.1}]])
      (b/preprocess-series)
      (b/update-scale :x :ticks 2)
      (b/update-scale :x :fmt "%,.0f")
      (b/add-axes :left)
      (b/add-axes :bottom)
      (b/add-label :bottom "population")
      (b/add-label :left "age")
      (r/render-lattice {:width 400 :height 400})
      (save "results/vega/bar-aggregate.jpg")
      (show)))

;; ;; https://vega.github.io/vega-lite/examples/bar_grouped.html

(-> (b/series [:stack-vertical [:bar population-male-female {:palette ["#EA98D2" "#659CCA"]}]])
    (b/preprocess-series)
    (b/update-scale :y :ticks 10)
    (b/update-scale :y :fmt "%,.0f")
    (b/add-axes :left)
    (b/add-axes :bottom)
    (b/add-label :top "age")
    (b/add-label :left "population")
    (b/add-legend "gender"
                  [[:rect "Female" {:color "#EA98D2"}]
                   [:rect "Male" {:color "#659CCA"}]])
    (r/render-lattice {:width 800 :height 400})
    (save "results/vega/bar-grouped.jpg")
    (show))

;; https://vega.github.io/vega-lite/examples/stacked_bar_weather.html

(let [selector (juxt :sun :snow :rain :fog :drizzle)
      data (->> seattle-weather
                (map #(vector (dt/format "MM" (first %)) (last %)))
                (frequencies)
                (group-by ffirst)
                (map-kv (fn [v]
                          (let [mm (into {} (map (fn [[[_ w] cnt]] [w cnt]) v))]
                            (map #(or % 0) (selector mm)))))
                (into (sorted-map)))
      legend (reverse (map #(vector :rect (name %2) {:color %1})
                           ["#e7ba52" "#9467bd" "#1f77b4" "#c7c7c7" "#aec7e8"]
                           [:sun :snow :rain :fog :drizzle]))]
  
  (-> (b/series
       [:grid nil {:x nil}]
       [:stack-vertical [:sbar data {:palette ["#e7ba52" "#9467bd" "#1f77b4" "#c7c7c7" "#aec7e8"]}]])
      (b/preprocess-series)
      (b/update-scale :x :scale [:bands {:padding-in 0.0 :padding-out 0.2}])
      (b/update-scale :y :ticks 10)
      (b/update-scale :y :fmt "%,.0f")
      (b/add-axes :left)
      (b/add-axes :bottom)
      (b/add-label :bottom "Month of the year")
      (b/add-label :left "Count of Records")
      (b/add-legend "Weather type" legend)
      (r/render-lattice {:width 500 :height 300}) 
      (save "results/vega/stacked-bar-weather.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/stacked_bar_h.html

(let [varietes (map keyword (sort-by (comp int first) > (distinct (map :site barley))))
      selector (apply juxt varietes)
      m (apply assoc {} (interleave varietes (repeat 0)))
      data (->> barley
                (group-by :variety)
                (map-kv (fn [v]
                          (selector (reduce (fn [m {:keys [site yield]}] 
                                              (update m (keyword site) + yield)) m v))))
                (sort-by (comp int first first) >))
      pal (reverse (take 6 (c/palette-presets :tableau-10-2)))
      legend (map #(vector :rect (name %2) {:color %1}) pal varietes)]
  
  (-> (b/series
       [:grid nil {:y nil}]
       [:stack-horizontal [:sbar data {:palette pal}]])
      (b/preprocess-series)
      (b/update-scale :x :ticks 5)
      (b/add-axes :left)
      (b/add-axes :bottom)
      (b/add-label :left "variety")
      (b/add-label :bottom "Sum of yield")
      (b/add-legend "site" legend)
      (r/render-lattice {:width 600 :height 300})
      (save "results/vega/stacked-bar-h.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/stacked_bar_normalize.html

(let [data (map-kv (fn [[x y]] [y x]) population-male-female)]
  (-> (b/series [:stack-vertical [:sbar data {:method :normalized :palette ["#659CCA" "#EA98D2"] :stroke? false}]])
      (b/preprocess-series)
      (b/add-axes :left)
      (b/add-axes :bottom)
      (b/add-label :left "population")
      (b/add-label :bottom "age")
      (b/add-legend "gender"
                    [[:rect "Female" {:color "#EA98D2"}]
                     [:rect "Male" {:color "#659CCA"}]])
      (r/render-lattice {:width 600 :height 300})     
      (save "results/vega/stacked-bar-normalize.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/bar_gantt.html

(let [data (sorted-map-by (comp - compare) :A [1 3] :B [3 8] :C [8 10])]
  (-> (b/series
       [:grid]
       [:stack-horizontal [:rbar data {:padding 0.05}]])
      (b/preprocess-series)
      (b/update-scale :y :fmt name)
      (b/add-axes :left)
      (b/add-axes :bottom)
      (b/add-label :bottom "start, end")
      (b/add-label :left "task")
      (r/render-lattice {:width 400 :height 200 :border 20})
      (save "results/vega/bar-gantt.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/bar_color_disabled_scale.html

(let [data [[:red 28] [:green 55] [:blue 43]]
      colors (map first data)]
  (-> (b/series [:stack-vertical [:bar data {:color (fn [_ {:keys [id]}] id)}]])
      (b/preprocess-series)
      (b/update-scale :x :fmt name)
      (b/add-axes :left)
      (b/add-axes :bottom)
      (b/add-label :bottom "color")
      (b/add-label :left "b")
      (r/render-lattice {:width 200 :height 400})
      (save "results/vega/bar-color.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/bar_layered_transparent.html

(let [data (map-kv (fn [[x y]] [y x]) population-male-female)]
  (-> (b/series
       [:grid nil {:x nil}]
       [:stack-vertical [:sbar data {:method :layered :palette (map #(c/color % 175) ["#659CCA" "#EA98D2"]) :stroke? false}]])
      (b/preprocess-series)
      (b/update-scale :y :fmt int)
      (b/add-axes :left)
      (b/add-axes :bottom)
      (b/add-label :left "population")
      (b/add-label :bottom "age")
      (b/add-legend "gender"
                    [[:rect "Female" {:color "#EA98D2"}]
                     [:rect "Male" {:color "#659CCA"}]])
      (r/render-lattice {:width 600 :height 300})
      (save "results/vega/stacked-bar-layer.jpg")
      (show)))

;; not done
;; https://vega.github.io/vega-lite/examples/bar_diverging_stack_transform.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SCATTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://vega.github.io/vega-lite/examples/point_2d.html

(let [data (filter (partial every? identity) (map (juxt :Horsepower :Miles_per_Gallon) cars))]
  (-> (b/series [:grid] [:scatter data {:shape \o :size 6 :stroke {:size 2}}])
      (b/preprocess-series)
      (b/update-scale :x :fmt int)
      (b/update-scale :y :fmt int)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "Horsepower")
      (b/add-label :left "Miles_per_Gallon")
      (r/render-lattice {:width 400 :height 400})
      (save "results/vega/point2d.jpg")
      (show)))


;; https://vega.github.io/vega-lite/examples/tick_dot.html

(let [data (map second seattle-weather)]
  (-> (b/series [:grid nil {:y nil}] [:rug data {:size 2}])
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-side :bottom (b/series [:label "precipitation"]))
      (r/render-lattice {:width 400 :height 90})
      (save "results/vega/tick-dot.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/tick_strip.html

(let [data (->> (map (juxt :Cylinders :Horsepower) cars)
                (filter (partial every? identity))
                (group-by first)
                (map-kv (fn [v] (map second v)))
                (into (sorted-map-by >)))]
  
  (-> (b/series [:grid] [:stack-horizontal [:rug data {:size 1 :color (c/color blue 250)}]])
      (b/preprocess-series)
      (b/update-scale :x :ticks 3)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "Horsepower")
      (b/add-label :left "Cylinders")
      (r/render-lattice {:width 300 :height 200})
      (save "results/vega/tick-strip.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/point_color_with_shape.html

(let [countries (sort (distinct (map :Origin cars)))
      markers [\o \s \^]
      pal (c/palette-presets :tableau-10-2)
      origins (apply assoc {} (interleave countries markers))
      colors (apply assoc {} (interleave countries pal))
      legend (map #(vector :shape %1 {:color %2 :shape %3 :size 8 :stroke {:size 2}}) countries pal markers)
      data (filter (partial every? identity) (map (juxt :Horsepower :Miles_per_Gallon :Origin) cars))]

  (-> (b/series [:grid] [:scatter data {:shape (fn [[_ _ v] _] (origins v)) :size 8 :stroke {:size 2}
                                        :color (fn [[_ _ v] _] (c/set-alpha (colors v) 200))}])
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "Horsepower")
      (b/add-label :left "Miles_per_Gallon")
      (b/add-legend "Origin" legend)
      (r/render-lattice {:width 400 :height 400})
      (save "results/vega/color-with-shape.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/circle_binned.html

(let [data (filter (partial every? identity) (map (juxt :IMDB_Rating :Rotten_Tomatoes_Rating) movies))]
  (-> (b/series [:gbubble data {:shape \O :size-range [1 35] :stroke {:size 2}}])
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "IMDB_Rating (binned)")
      (b/add-label :left "Rotten_Tomatoes_Rating (binned)")
      (r/render-lattice {:width 400 :height 400})
      (save "results/vega/circle-binned.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/point_bubble.html

(let [data (filter (partial every? identity) (map (juxt :Horsepower :Miles_per_Gallon :Acceleration) cars))]
  (-> (b/series [:grid] [:bubble data {:shape \o :size-range [1 30] :stroke {:size 2}}])
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "Horsepower")
      (b/add-label :left "Miles_per_Gallon")
      (r/render-lattice {:width 400 :height 400})
      (save "results/vega/point-bubble.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/point_invalid_color.html

(let [alldata (map (juxt :IMDB_Rating :Rotten_Tomatoes_Rating) movies)
      data (remove (partial some nil?) alldata)
      nils-x (filter some? (map first (filter (comp not second) alldata)))
      nils-y (filter some? (map second (filter (comp not first) alldata)))
      strip-conf {:size 6 :distort 0.2 :color (c/color :gray 50)}]
  
  (-> (b/series [:grid] [:scatter data {:distort 100 :shape \o :size 6 :stroke {:size 2}}])
      (b/preprocess-series)
      (b/add-side :top 10 (b/series [:strip nils-x strip-conf]))
      (b/add-side :right 10 (b/series [:strip nils-y strip-conf]))
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "IMDB_Rating")
      (b/add-label :left "Rotten_Tomatoes_Rating")
      (r/render-lattice {:width 400 :height 400})
      (save "results/vega/nulls.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/circle.html

(let [data (filter (partial every? identity) (map (juxt :Horsepower :Miles_per_Gallon :Acceleration) cars))]
  (-> (b/series [:grid] [:scatter data {:shape \O :size 8 :stroke {:size 0}}])
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "Horsepower")
      (b/add-label :left "Miles_per_Gallon")
      (r/render-lattice {:width 400 :height 400})
      (save "results/vega/circles.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/circle_bubble_health_income.html

(let [data (filter (partial every? identity) (map rest gapminder-health))]
  (-> (b/series [:grid] [:bubble data {:scale-z [:pow 0.7] :shape \O
                                       :color (c/color :black 180) :size-range [0.5 25]
                                       :stroke {:size 0}
                                       :margins {:x [0.001 0]}}])
      (b/preprocess-series)
      (b/update-scale :x :scale [:log])
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "income")
      (b/add-label :left "health")
      (r/render-lattice {:width 500 :height 300})
      (save "results/vega/circle-bubble-hi.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/circle_natural_disasters.html

(first disasters)
;; => ["All natural disasters" 1900 1267360]

(let [pal (cycle (reverse (c/palette-presets :tableau-10-2)))
      filtered-disasters (filter #(not= "All natural disasters" (first %)) disasters)
      deaths (s/pow 0.5 (stats/extent (map #(nth % 2) filtered-disasters)))
      data (->> filtered-disasters
                (group-by first)
                (map-kv (fn [v] (map rest v)))
                (into (sorted-map-by (comp - compare))))]
  
  (-> (b/series [:stack [true :strip data {:color (fn [_ {:keys [series-id]}] (c/set-alpha (nth pal series-id) 200))
                                           :size (fn [[_ d] _] (deaths 1 65 d))}]])
      (b/preprocess-series)
      (b/update-scale :x :fmt int)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "year")
      (r/render-lattice {:width 600 :height 400})
      (save "results/vega/circle-natural-disasters.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/text_scatterplot_colored.html

(let [countries (sort (distinct (map :Origin cars)))
      markers ["E" "J" "U"]
      pal (c/palette-presets :tableau-10-2)
      origins (apply assoc {} (interleave countries markers))
      colors (apply assoc {} (interleave countries pal))
      legend (map #(vector :shape %1 {:color %2 :shape %3 :size 18 :stroke {:size 2}}) countries pal markers)
      data (filter (partial every? identity) (map (juxt :Horsepower :Miles_per_Gallon :Origin) cars))]
  
  (-> (b/series [:grid] [:scatter data {:shape (fn [[_ _ v] _] (origins v)) :size 18 :stroke {:size 2}
                                        :color (fn [[_ _ v] _] (c/set-alpha (colors v) 180))}])
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "Horsepower")
      (b/add-label :left "Miles_per_Gallon")
      (b/add-legend "Origin" legend)
      (r/render-lattice {:width 450 :height 400})
      (save "results/vega/text-scatterplot-colored.jpg")
      (show)))

;;;;;;;;;;;;;;;;;;;;;;;
;;;; LINES
;;;;;;;;;;;;;;;;;;;;;;;

;; https://vega.github.io/vega-lite/examples/line.html

(let [data (->> (filter #(= :GOOG (first %)) stocks)
                (map rest))]
  (-> (b/series [:grid] [:line data {:stroke {:size 2} }])
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "date")
      (b/add-label :left "price")
      (r/render-lattice {:width 600 :height 400})
      (save "results/vega/line.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/line_overlay.html
;; https://vega.github.io/vega-lite/examples/line_overlay_stroked.html

(let [data (->> stocks
                (group-by first)
                (map-kv (fn [v]
                          (->> (group-by (comp #(dt/as % :year) second) v)
                               (map-kv (fn [v]
                                         (stats/mean (map last v))))
                               (into [])
                               (sort-by first)))))
      pal (c/palette-presets :tableau-10-2)
      legend (map #(vector :line (name %2) {:color %1 :shape \O}) pal (keys data))]
  (-> (b/series [:grid])
      (b/add-multi :line data {:stroke {:size 2} :point {:type \O}} {:color pal})
      (b/preprocess-series)
      (b/update-scale :x :fmt int)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "date (year)")
      (b/add-label :left "Mean of price")
      (b/add-legend "symbol" legend)
      (r/render-lattice {:width 600 :height 400})
      (save "results/vega/line-overlay.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/line_color.html

(let [data (->> stocks
                (group-by first)
                (map-kv (fn [v] (sort-by first (map rest v)))))
      pal (c/palette-presets :tableau-10-2)
      legend (map #(vector :line (name %2) {:color %1 :stroke {:size 2}}) pal (keys data))]
  (-> (b/series [:grid])
      (b/add-multi :line data {:stroke {:size 2}} {:color pal})
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "date")
      (b/add-label :left "price")
      (b/add-legend "symbol" legend)
      (r/render-lattice {:width 600 :height 400})
      (save "results/vega/line-color.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/line_step.html

(let [data (->> (filter #(= :GOOG (first %)) stocks)
                (map rest))]
  (-> (b/series [:grid] [:line data {:stroke {:size 2} :interpolation in/step-before}])
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "date")
      (b/add-label :left "price")
      (r/render-lattice {:width 600 :height 400})
      (save "results/vega/line-step.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/line_monotone.html

(let [data (->> (filter #(= :GOOG (first %)) stocks)
                (map rest))]
  (-> (b/series [:grid] [:line data {:stroke {:size 2} :interpolation in/monotone}])
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "date")
      (b/add-label :left "price")
      (r/render-lattice {:width 600 :height 400})
      (save "results/vega/line-spline.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/connected_scatterplot.html

(let [data (sort-by last (map (juxt :miles :gas :year) driving))]
  (-> (b/series [:grid] [:line data {:stroke {:size 2} :point {:type \O}}])
      (b/preprocess-series)
      (b/update-scale :x :ticks 4)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "miles")
      (b/add-label :left "gas")
      (r/render-lattice {:width 400 :height 400 :border 20})
      (save "results/vega/connected-scatterplot.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/trail_color.html

(let [data (->> stocks
                (group-by first)
                (map-kv (fn [v] (sort-by first (map rest v)))))
      pal (c/palette-presets :tableau-10-2)
      legend (map #(vector :line (name %2) {:color %1 :stroke {:size 2}}) pal (keys data))]
  (-> (b/series [:grid])
      (b/add-multi :line data {:point {:type \O :size (fn [[_ size] _] (/ size 50.0))}} {:color pal})
      (b/preprocess-series)
      (b/update-scale :x :ticks 4)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "date")
      (b/add-label :left "price")
      (b/add-legend "symbol" legend)
      (r/render-lattice {:width 600 :height 400})
      (save "results/vega/trail-color.jpg")
      (show)))


;; https://vega.github.io/vega-lite/examples/layer_line_co2_concentration.html

(let [co2-date->number (fn [d]
                         (let [yd (mod (dt/as d :year) 10)
                               m (dec (dt/as d :month-of-year))]
                           (+ yd (/ m 12.0))))
      data (->> co2-concentration
                (group-by (comp #(m/floor (/ (dt/as % :year) 10.0)) first))
                (sort-by first)
                (map (fn [[k v]] [k (sort-by first (map (fn [[d co2]] [(co2-date->number d) co2]) v))]))
                (into (sorted-map)))]
  (-> (b/series [:grid])
      (b/add-multi :line data {:stroke {:size 2}} {:color (c/resample (inc (count data)) (c/palette-presets :viridis-magma))})
      (b/preprocess-series)
      (b/update-scale :x :fmt int)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "Year into Decade")
      (b/add-label :left "CO2 concentration in ppm")
      (r/render-lattice {:width 600 :height 400})
      (save "results/vega/layer-line-co2-concentration.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/area.html

(let [data (->> unemployment
                (group-by (fn [v]
                            (dt/local-date (:year v) (:month v))))
                (map-kv (fn [v]
                          (reduce + (map :count v))))
                (sort-by first))]
  (-> (b/series [:grid] [:area data])
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "date (year-month)")
      (b/add-label :left "count")
      (r/render-lattice {:width 600 :height 400})
      (save "results/vega/area.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/area_overlay.html

(let [data (->> (filter #(= :GOOG (first %)) stocks)
                (map rest))]
  (-> (b/series [:grid] [:area data {:stroke {:size 2} :point {:type \O}}])
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "date")
      (b/add-label :left "price")
      (r/render-lattice {:width 600 :height 400})
      (save "results/vega/area-overlay.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/stacked_area.html

(let [pal (cycle (c/palette-presets :category20b))
      legend (reverse (map #(vector :rect %2 {:color %1}) pal (keys unemployment-area)))]
  (-> (b/series [:grid] [:sarea unemployment-area])
      (b/preprocess-series)
      (b/update-scale :y :fmt int)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "date (year-month)")
      (b/add-label :left "Sum of count")
      (b/add-legend "series" legend)
      (r/render-lattice {:width 800 :height 400})
      (save "results/vega/stacked-area.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/stacked_area_normalize.html

(let [pal (cycle (c/palette-presets :category20b))
      legend (reverse (map #(vector :rect %2 {:color %1}) pal (keys unemployment-area)))]
  (-> (b/series [:grid] [:sarea unemployment-area {:method :normalized}])
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-label :bottom "date (year-month)")
      (b/add-legend "series" legend)
      (r/render-lattice {:width 800 :height 400})
      (save "results/vega/stacked-area-normalize.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/stacked_area_stream.html

(let [pal (cycle (c/palette-presets :category20b))
      legend (reverse (map #(vector :rect %2 {:color %1}) pal (keys unemployment-area)))]
  (-> (b/series [:grid] [:sarea unemployment-area {:method :stream}])
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-label :bottom "date (year-month)")
      (b/add-legend "series" legend)
      (r/render-lattice {:width 800 :height 400})
      (save "results/vega/stacked-area-stream.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/layer_point_errorbar_ci.html

(-> (b/series [:grid nil {:y nil}]
              [:stack-horizontal [:extent-stat barley-variety-yield {:extent-type :bci :shape \O :size 5 :stroke {:size 1} :color :black}]])
    (b/preprocess-series)
    (b/update-scale :x :ticks 5)
    (b/add-axes :bottom)
    (b/add-axes :left)
    (b/add-label :bottom "Barley Yield")
    (b/add-label :left "variety")
    (r/render-lattice {:width 400 :height 400})
    (save "results/vega/point-errorbar-ci.jpg")
    (show))


;; https://vega.github.io/vega-lite/examples/layer_point_errorbar_stdev.html

(-> (b/series [:grid nil {:y nil}]
              [:stack-horizontal [:extent-stat barley-variety-yield {:extent-type :stddev :shape \O :size 5 :stroke {:size 1} :color :black}]])
    (b/preprocess-series)
    (b/update-scale :x :ticks 5)
    (b/add-axes :bottom)
    (b/add-axes :left)
    (b/add-label :bottom "Barley Yield")
    (b/add-label :left "variety")
    (r/render-lattice {:width 400 :height 400})
    (save "results/vega/point-errorbar-stddev.jpg")
    (show))


;; https://vega.github.io/vega-lite/examples/boxplot_2D_vertical.html
;; https://vega.github.io/vega-lite/examples/boxplot_minmax_2D_vertical.html

(let [data (->> population
                (group-by :age)
                (map-kv #(map :people %))
                (sort-by first))]
  (-> (b/series [:grid nil {:x nil}] [:stack [false :box data {:outliers? true :shape \O}]])
      (b/preprocess-series)
      (b/update-scale :y :fmt int)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "age")
      (b/add-label :left "population")
      (r/render-lattice {:width 600 :height 400})
      (save "results/vega/box-plot.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/rect_binned_heatmap.html

(let [data (map (juxt :IMDB_Rating :Rotten_Tomatoes_Rating) movies)]
  (-> (b/series [:heatmap data {:grid :square :size 10 :gradient (c/gradient (c/palette-presets :ylgnbu-9))}])
      (b/preprocess-series)
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "IMDB_Rating (binned)")
      (b/add-label :left "Rotten_Tomatoes_Rating (binned)")
      (r/render-lattice {:width 600 :height 400})
      (save "results/vega/table-binned-heatmap.jpg")
      (show)))

;; https://vega.github.io/vega-lite/examples/concat_marginal_histograms.html

(let [data (map (juxt :IMDB_Rating :Rotten_Tomatoes_Rating) movies)]
  (-> (b/series [:heatmap data {:grid :square :size 20 :gradient (c/gradient (c/palette-presets :ylgnbu-9))}])
      (b/preprocess-series)
      (b/add-side :right (b/series [:histogram (remove nil? (map second data)) {:bins 10}]))
      (b/add-side :top (b/series [:histogram (remove nil? (map first data)) {:bins 10}]))
      (b/add-axes :bottom)
      (b/add-axes :left)
      (b/add-label :bottom "IMDB_Rating (binned)")
      (b/add-label :left "Rotten_Tomatoes_Rating (binned)")
      (r/render-lattice {:width 400 :height 400})
      (save "results/vega/table-binned-heatmap-marginal.jpg")
      (show)))
