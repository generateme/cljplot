(ns cljplot.config
  (:require [clojure2d.color :as c]
            [cljplot.common :refer [deep-merge]]))

(def ^:private blue (last (:rdylbu-9 c/palette-presets)))
(def ^:private red (first (:rdylbu-9 c/palette-presets)))
(def ^:private dblue (c/darken blue))
(def ^:private dred (c/darken red))

(def ^:private configuration-functions {:histogram [:color [:stroke :color]]
                                        :axis-bottom [[:ticks :size]]
                                        :axis-top [[:ticks :size]]
                                        :axis-left [[:ticks :size]]
                                        :axis-right [[:ticks :size]]
                                        :rug [:color :size]
                                        :strip [:color :size]
                                        :density-strip [:color]
                                        :scatter [:color :size [:stroke :size] :shape]
                                        :bubble [:color [:stroke :size] :shape]
                                        :gbubble [:color [:stroke :size] :shape]
                                        :bar [:color [:stroke :color]]
                                        :sbar [:color [:stroke :color]]
                                        :rbar [:color]
                                        :line [[:point :size]]})

(def ^:private axes-config
  (let [base {:line {:color :black
                     :stroke {:size 1.0 :cap :butt}
                     :angle 0.0}
              :ticks {:size 6
                      :type :line
                      :color :black
                      :stroke {:size 1.0 :cap :butt}
                      :anchor -1.0
                      :font-size 10.0
                      :font nil
                      :text-angle 0.0
                      :shift-x 0.0
                      :shift-y-rel 1.0
                      :text-align :center}}]
    {:axis-bottom base
     :axis-top (-> base
                   (assoc-in [:ticks :anchor] 1.0)
                   (assoc-in [:ticks :shift-y-rel] -0.5))
     :axis-left (-> base
                    (assoc-in [:line :angle] -90.0)
                    (assoc-in [:ticks :anchor] 1.0)
                    (assoc-in [:ticks :shift-x] -5.0)
                    (assoc-in [:ticks :shift-y-rel] 0.0)
                    (assoc-in [:ticks :text-align] :right))
     :axis-right (-> base
                     (assoc-in [:line :angle] -90.0)
                     (assoc-in [:ticks :shift-x] 5.0)
                     (assoc-in [:ticks :shift-y-rel] 0.0)
                     (assoc-in [:ticks :text-align] :left))}))

(def ^:private stroke-common {:color nil :size 1.0 :cap :butt})

(def ^:private configuration (merge axes-config
                                    {:histogram {:color blue
                                                 :palette (cycle (c/palette-presets :category20))
                                                 :stroke stroke-common
                                                 :percents? true
                                                 :stroke? true
                                                 :type :bars
                                                 :padding-in 0.1
                                                 :padding-out 0.2
                                                 :bins nil
                                                 :margins {:x [0.05 0.05] :y [0.0 0.01]}}
                                     :grid {:x {:color (c/color :gray 180)
                                                :stroke {:size 0.5 :cap :butt :dash [4.0] :dash-phase 2.0}}
                                            :y {:color (c/color :gray 180)
                                                :stroke {:size 0.5 :cap :butt :dash [4.0] :dash-phase 2.0}}}
                                     :rug {:color (c/set-alpha blue 100)
                                           :size 1.0
                                           :cap :butt
                                           :scale 1.0
                                           :distort 0.0
                                           :margins {:x [0.05 0.05]}}
                                     :strip {:color (c/set-alpha blue 50)
                                             :size 10.0
                                             :distort 0.0
                                             :scale 0.25
                                             :shape \O
                                             :margins {:x [0.05 0.05]}}
                                     :extent-stat {:extent-type :min-max
                                                   :color blue
                                                   :shape \A
                                                   :size 10.0
                                                   :stroke {:size 3.0 :cap :butt}
                                                   :margins {:x [0.05 0.05]}}
                                     :box {:color blue
                                           :shape \O
                                           :outliers? true
                                           :size 1.0
                                           :margins {:x [0.05 0.05]}}
                                     :violin {:color blue
                                              :color-bar (c/darken dblue)
                                              :size-bar 5.0
                                              :size 1.0
                                              :normalize? true
                                              :scale 0.85
                                              :margins {:x [0.05 0.05]}
                                              :kernel-bandwidth nil}
                                     :density-strip {:color blue
                                                     :size 1.0
                                                     :normalize? true
                                                     :kernel-bandwidth nil
                                                     :scale 0.9
                                                     :fill? false}
                                     :scatter {:color (c/set-alpha blue 180)
                                               :shape \O
                                               :stroke {:size 1.0}
                                               :size 4.0
                                               :margins {:x [0.05 0.05] :y [0.05 0.05]}}
                                     :bubble {:color (c/set-alpha blue 180)
                                              :shape \O
                                              :stroke {:size 1.0}
                                              :size-range [2.0 10]
                                              :scale-z [:pow 0.5]
                                              :margins {:x [0.05 0.05] :y [0.05 0.05]}}
                                     :gbubble {:color (c/set-alpha blue 180)
                                               :shape \O
                                               :stroke {:size 1.0}
                                               :size-range [2.0 10]
                                               :scale-z [:pow 0.5]
                                               :cells 10
                                               :grid-type :square
                                               :margins {:x [0.05 0.05] :y [0.05 0.05]}}
                                     :line {:color blue
                                            :stroke {:size 1.0}
                                            :interpolation nil
                                            :smooth? false
                                            :margins {:x [0.05 0.05] :y [0.05 0.05]}
                                            :point {:type nil :size 6}}
                                     :function {:domain [0 1]
                                                :samples nil
                                                :color blue
                                                :stroke {:size 1.0}
                                                :interpolation nil
                                                :margins {:x [0.05 0.05] :y [0.05 0.05]}
                                                :smooth? false}
                                     :density {:domain [0 1]
                                               :samples nil
                                               :color blue
                                               :stroke {:size 1.0}
                                               :interpolation nil
                                               :smooth? false
                                               :kernel-bandwidth nil
                                               :margins {:x [0.1 0.1] :y [0.0 0.05]}
                                               :area? false}
                                     :bar {:color (fn [v _] (if (neg? v) red blue))
                                           :stroke? true
                                           :stroke {:color (fn [v _] (if (neg? v) dred dblue))
                                                    :size 1.0}
                                           :padding-in 0.1
                                           :padding-out 0.2
                                           :margin 0.0
                                           :palette (c/palette-presets :category20)}
                                     :rbar {:color blue
                                            :stroke? true
                                            :stroke {:size 1.0}
                                            :padding 0.1}
                                     :sbar {:stroke? true
                                            :stroke {:size 1.0}
                                            :padding 0.1
                                            :margin 0.0
                                            :method :stacked
                                            :palette (c/palette-presets :category20)}}))

(def aliases {:area :line
              :cdf :function
              :ppplot :scatter
              :qqplot :ppplot})

(defn- val->fn
  "Convert value to function if it's not a function."
  [v]
  (if (or (nil? v) (fn? v)) v (constantly v)))

(defn- coerce-fn
  "For given map `m` and `keys` convert selected values to functions.

  `keys` can be single key or sequence of keys."
  [m & keys]
  (when m
    (reduce #(if (sequential? %2)
               (assoc-in %1 %2 (val->fn (get-in %1 %2)))
               (assoc %1 %2 (val->fn (get %1 %2)))) m keys)))

(defn merge-configuration
  "Merge two configs and convert selected values to keys." 
  [chart-type config]
  (if-let [alias (aliases chart-type)]
    (merge-configuration alias config)
    (apply coerce-fn (deep-merge (configuration chart-type) config) (configuration-functions chart-type))))

(defn get-configuration
  [chart-type]
  (merge-configuration chart-type {}))

