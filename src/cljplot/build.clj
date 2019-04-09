(ns cljplot.build
  (:require [cljplot.common :refer :all]
            [cljplot.config :refer :all]
            [cljplot.axis :as axis]
            [cljplot.scale :as s]
            [clojure2d.color :as c]
            [fastmath.random :as r]
            [cljplot.build :as b]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

;;

(defn series
  "Pack series into list"
  [& series] (vec series))

(defn add-serie
  "Append series"
  ([series [t d c] x y]
   (conj series [t d (assoc c :position [x y])]))
  ([series serie]
   (add-serie series serie 0 0)))

(def add-series (comp vec concat))

(defn add-multi
  "Create definition for multi datasets from a map for overlays and append"
  ([series serie-name map-of-data] (add-multi series serie-name map-of-data {}))
  ([series serie-name map-of-data serie-config] (add-multi series serie-name map-of-data serie-config {}))
  ([series serie-name map-of-data serie-config multi-config]
   (let [mconfig (map-kv cycle multi-config)]
     (reduce conj series
             (for [[idx [k d]] (map-indexed vector map-of-data)
                   :let [nconfig (reduce-kv (fn [m ky v]
                                              ((if (sequential? ky) assoc-in assoc) m ky (nth v idx))) serie-config multi-config)]]
               [serie-name d (assoc nconfig :serie-name k)])))))

(defn- find-most-squared-shape
  "Let's find best squared shape for given number of data"
  [^long v]
  (let [s (m/sqrt v)
        r (range 1 (inc v))
        [_ ^long x ^long y] (first (sort-by first clojure.core/< (for [^long x r ^long y r
                                                                       :let [d1 (- (* x y) v)
                                                                             d (+ d1 d1 (+ (m/sq (- s x))
                                                                                           (m/sq (- s y))))]
                                                                       :when (>= d1 0.0)]
                                                                   [d x y])))]
    (if (< x y) [x y] [y x])))

(def ^:private find-most-squared-shape+ (memoize find-most-squared-shape))

(defn- ensure-rows-cols
  [shape ^long total]
  (let [total (double total)
        [^long rows ^long cols] (map #(when (and % (pos? ^long %)) %) shape)]
    (cond
      (and rows cols) [rows cols]
      rows [rows (int (m/ceil (/ total rows)))]
      cols [(int (m/ceil (/ total cols))) cols]
      :else (find-most-squared-shape+ total))))

(defn lattice
  "Pack series into lattice"
  ([serie-type data] (lattice serie-type data nil))
  ([serie-type data serie-config] (lattice serie-type data serie-config nil))
  ([serie-type data serie-config conf]
   (let [[rows cols] (ensure-rows-cols (:shape conf) (count data))
         positions (for [y (range rows) x (range cols)] [x y])
         grid-conf (when-let [gc (:grid conf)]
                     (if (map? (:grid conf)) (:grid conf) {}))
         label (when-let [lc (:label conf)]
                 (if (string? lc) (partial format lc) lc))]
     (mapcat (fn [[k v] p]
               (let [with-label (if label (assoc serie-config :label (label k)) serie-config)
                     s [serie-type v (assoc with-label :position p)]]
                 (if-not grid-conf [s]
                         [[:grid nil (assoc grid-conf :position p)] s]))) data positions))))

;;

(defn- add-default-position
  "Add default position to config"
  [[t d c]] [t d (if (contains? c :position) c (assoc c :position [0 0]))])

(defn- group-by-position
  "Create a map of series grouped by chart positions.
  Additionally find number of rows and cols. Default position is [0,0]."
  [srs]
  (let [gsrs (group-by (comp :position last) (map add-default-position srs))
        [cols rows] (map #(inc ^long (apply clojure.core/max (mapv % (keys gsrs)))) [first second])]
    {:series gsrs
     :cols cols
     :rows rows}))

(defn- add-series-info
  "Add series id and chart type to configuration"
  [id [t d c]] [t d (-> (merge-configuration t (or c {}))
                        (assoc :series-id id :chart-type t))])

(defn- chart-process-data
  "Prepare data for given chart type"
  [f [t d c]] [t (f t d c) c])

(defn- calculate-extent
  [[t d c]] [t d (assoc c :extent (extend-domains (data-extent t d c) (:margins c)))])

(defn- preprocess-steps
  [s]
  (into [] (comp
            (map-indexed add-series-info) ;; add series-id
            (map (partial chart-process-data prepare-data)) ;; precalculate data
            (map calculate-extent)) ;; calculate extents
        s))

(defn- adjust-configs-and-preprocess
  "Insert additional information to config and merge with defaults.
  Calculate extents and preprocess data in one step."
  [srs]
  (update srs :series #(map-kv preprocess-steps %)))

;; awful part...

(defn- select-row-col-ids
  "Select keys for given row (1) or column (0)."
  [k ^long id m]
  (filter #(== id ^long (% k)) (keys m)))

(defn- merge-extents-by-ids
  "Merge extents by column/row"
  [exts cnt pos k]
  (into {} (->> (range cnt)
              (map (fn [id]
                     [id (->> exts
                            (select-row-col-ids pos id)
                            (mapcat exts)
                            (map k)
                            (find-min-max))]))
              (remove (comp nil? second)))))

;; TODO: merge all other extents globally (like :inner)

(defn- merge-extents
  "Find biggest extent for each grid position."
  [{:keys [rows cols] :as srs}]
  (let [exts (map-kv (partial map (comp :extent last)) (:series srs))]
    (assoc srs :extents {:x (merge-extents-by-ids exts cols 0 :x)
                         :y (merge-extents-by-ids exts rows 1 :y)})))

;;

(def ^:private default-scale-defs
  {:numerical [:linear]
   :temporal [:time]
   :categorical [:bands]})

(defn- auto-scale
  ([series x-or-y]
   (assoc-in series [:scales x-or-y] (->> (get-in series [:extents x-or-y])
                                          (map-kv (fn [[t domain conf]]
                                                    (let [t (get default-scale-defs t [:linear])]
                                                      (s/scale-map (if conf (conj t conf) t)
                                                                   {:domain domain}))))))))

(defn- auto-scales
  [series]
  (-> series
      (auto-scale :x)
      (auto-scale :y)))

(defn preprocess-series
  [series]
  (-> series
     (group-by-position)
     (adjust-configs-and-preprocess)
     (merge-extents)
     (auto-scales)))

(defn update-scale
  ([series axis k v] (update-scale series axis 0 k v))
  ([series axis pos k v]
   (update-in series [:scales axis pos] s/update-scale k v)))

(defn update-scales
  [series axis k v]
  (reduce #(update-scale %1 axis %2 k v) series (-> series :scales axis keys)))

(defn tie-domains
  [series & axes]
  (reduce #(update-scales %1 %2 :domain (second (find-min-max (-> %1 :extents %2 vals)))) series  axes))

(defn- find-size
  [side-nseries]
  (let [s (remove nil? (map (comp :block-size second) side-nseries))]
    (when (seq s) (reduce fast-max s))))

(defn- append-side
  [series side pos size side-nseries]
  (let [s (update (get series side {pos []}) pos conj {:size (or (find-size side-nseries) size) :series side-nseries})]
    (assoc series side s)))

(def ^:private position->scale-id {:bottom :x :top :x :left :y :right :y})

(defn add-axes
  ([series side] (add-axes series side {}))
  ([series side config]
   (let [conf-name (keyword (str "axis-" (name side)))
         config (merge-configuration conf-name config)
         scale-id (position->scale-id side)]
     (reduce (fn [s [pos scale]]
               (let [size (axis/axis-size scale scale-id config)]
                 (append-side s side pos size [[:axis scale-id config]]))) series (get (:scales series) scale-id)))))

(defn add-side
  ([series side side-series] (add-side series side 50 side-series))
  ([series side size side-series] (add-side series side 0 size side-series))
  ([series side pos size side-series]
   (append-side series side pos size (preprocess-steps side-series))))

(defn add-label
  ([series side label] (add-label series side label {}))
  ([series side label conf]
   (let [conf (merge-configuration :label conf)
         data (prepare-data :label label conf)]
     (assoc-in series [:labels side] data))))

(defn add-legend
  [series name defs]
  (if (seq defs)
    (update series :legend update name (fn [v] (vec (if (seq v) (concat v defs) defs))))
    series))


;;;;;;;;;;;;;;;;;
