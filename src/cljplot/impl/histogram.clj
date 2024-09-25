(ns cljplot.impl.histogram
  (:require [cljplot.common :as common]
            [cljplot.scale :as s]
            [fastmath.core :as m]
            [fastmath.stats :as stats]
            [clojure2d.core :as c2d]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

#_(register-configuration! :histogram {:color blue
                                       :palette (cycle (c/palette-presets :category20))
                                       :stroke stroke-common
                                       :percents? true
                                       :stroke? true
                                       :type :bars
                                       :padding-in 0.1
                                       :padding-out 0.2
                                       :bins nil
                                       :margins {:x [0.05 0.05] :y [0.0 0.01]}})


(defmethod common/prepare-data :frequencies [_ data {:keys [pmf?]}]
  (let [s? (sequential? (first data))
        data (if s? data [data])
        freqs (mapv frequencies data)]
    (if-not pmf?
      freqs
      (let [sums (mapv #(reduce m/+ 0.0 (vals %)) freqs)]
        (map (fn [freq ^double s]
               (common/map-kv #(/ ^double % s) freq)) freqs sums)))))

(defmethod common/data-extent :frequencies [_ data {:keys [range? sort?]}]
  (let [ks (->> data
                (map keys)
                (flatten)
                (distinct))
        n? (every? integer? ks)]
    {:x [:categorical (if (and n? range?)
                        (map int (range (reduce m/min ks) (inc ^int (reduce m/max ks))))
                        (if sort? (sort ks) ks))]
     :y [:numerical [0 (->> data
                            (map vals)
                            (flatten)
                            (reduce m/max))]]}))

(defn- histogram-density
  [{:keys [^long samples ^double step] :as h} density?]
  (let [f (case density?
            :pmf (/ 1.0 samples)
            :absolute (/ step)
            (/ (* step samples)))]
    (update h :bins (partial map (fn [[x ^long s]] [x (* s f)])))))

(defn- cumulate
  [{:keys [bins ^double step] :as h} density?]
  (let [b (reductions m/+ (map second bins))
        step (if (or (not density?) (= :pmf density?)) 1.0 step)]
    (update h :bins (partial map (fn [^double c [x _]] [x (* step c)]) b))))

(defmethod common/prepare-data :histogram [_ data {:keys [bins density? cumulative?]}]
  (let [s? (sequential? (first data))
        fd (if s? (flatten data) data)
        b (stats/estimate-bins fd bins)
        e (stats/extent fd)
        hs (map #(cond-> (stats/histogram % b e)
                   density? (histogram-density density?)
                   cumulative? (cumulate density?))
                (if s? data [data]))]
    (-> (select-keys (first hs) [:min :max :step])
        (assoc :bins (map :bins hs)))))

(defmethod common/data-extent :histogram [_ data _]
  (let [d (map (partial map second) (:bins data))        
        max-bin (reduce m/max (flatten d))
        ^double mn (:min data)
        ^double mx (:max data)
        diff (- mx mn)]
    {:x [:numerical [(if (< diff 2.0) mn (m/floor mn))
                     (if (< diff 2.0) mx (m/ceil mx))]]
     :y [:numerical [0 max-bin]]}))

(defn- draw-series
  [canvas data {:keys [palette stroke stroke? type padding-in padding-out ^double zero]
                :or {type :bars}}]
  (let [bands (s/bands {:padding-out padding-out :padding-in padding-in} (count data))
        zero (+ zero 3)]
    (doseq [[idx d] (map-indexed vector data)]
      (let [{:keys [^double start ^double end ^double point]} (bands idx)
            col (nth palette idx)]
        (doseq [[^double x1 ^double x2 ^double y] d
                :let [diff (- x2 x1)
                      startx (+ x1 (* diff start))
                      endx (+ x1 (* diff end))
                      pointx (+ x1 (* diff point))
                      diffx (- endx startx)
                      diffy (max 0.0 (- y zero))]]
          (case type
            :lollipops (-> canvas
                           (c2d/set-color col)
                           (c2d/set-stroke-custom stroke)
                           (c2d/line pointx zero pointx y)
                           (c2d/ellipse pointx y diffx diffx))
            (if stroke?
              (-> canvas
                  (c2d/set-stroke-custom stroke)
                  (c2d/filled-with-stroke col (c/darken col) c2d/rect startx zero diffx diffy))
              (-> canvas
                  (c2d/set-color col)
                  (c2d/rect startx zero diffx diffy)))))))))

(defmethod common/render-graph :frequencies [_ data conf {:keys [^int w h x y] :as chart-data}]
  (let [sx (:scale x)
        scale-y (partial (:scale y) 0 h)
        d (map (partial map (fn [[x y]]
                              (let [{:keys [^double start ^double end]} (sx x)
                                    s (* start w)
                                    e (* end w)]
                                [s e (scale-y y)]))) data)]
    (common/do-graph chart-data false
      (draw-series c d (assoc conf :zero (scale-y 0.0))))))

(defmethod common/render-graph :histogram [_ {:keys [^double step bins]} conf {:keys [w h x y] :as chart-data}]
  (let [scale-x (partial (:scale x) 0 w)
        scale-y (partial (:scale y) 0 h)
        d (map (partial map (fn [[^double x y]] [(scale-x x) (scale-x (+ x step)) (scale-y y)])) bins)]
    (common/do-graph chart-data false
      (draw-series c d (assoc conf :zero (scale-y 0.0))))))

(m/unuse-primitive-operators)
