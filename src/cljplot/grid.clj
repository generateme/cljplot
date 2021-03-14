(ns cljplot.grid
  "Grid layout manipulation functions."
  (:require [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)
;; (m/unuse-primitive-operators)

(def default-borders {:inner 0 :outer 5 :gap 1})
(def default-cell {:borders default-borders
                   :left '()
                   :right []
                   :top '()
                   :bottom []})

(defn grid
  [rows cols]
  (apply merge (assoc default-cell :rows rows :cols cols)
         (for [r (range rows)
               c (range cols)]
           [[r c] default-cell])))

(defn- infer-indices-seq
  [cnt selector]
  (cond
    (number? selector) [selector]
    (seq selector) (distinct selector)
    :else (range cnt)))

(defn- get-indices
  [{:keys [rows cols]} row col]
  (for [r (infer-indices-seq rows row)
        c (infer-indices-seq cols col)]
    [r c]))

(defn- pack-side-data
  [side-data]
  (if (number? side-data)
    {:size side-data}
    side-data))

(defn add-side
  ([grid side side-data]
   (update grid side conj (pack-side-data side-data)))
  ([grid row col side side-data]
   (reduce (fn [g idx]
             (update-in g [idx side] conj (pack-side-data side-data))) grid (get-indices grid row col))))

(defn update-border
  ([grid borders]
   (update grid :borders merge borders))
  ([grid row col borders]
   (reduce (fn [g idx]
             (update-in g [idx :borders] merge borders)) grid (get-indices grid row col))))


;; calc sizes

(def ^:private side-order [:left :right :top :bottom])
(def ^:private side-selector (apply juxt side-order))

(defn- fix-borders
  "Ensure borders is a map with value for each side."
  [borders]
  (cond
    (map? borders) (merge (zipmap side-order (repeat 0)) borders)
    (number? borders) (zipmap side-order (repeat borders))
    :else (zipmap side-order (concat borders (repeat 0)))))

(defn- maybe-multiply
  "If value is percentage multiply by total size (width or height)."
  ^long [^double v ^long vv]
  (-> (if (< v 1.0) (* v vv) v)
      (m/floor)
      (long)))

(defn- border-sizes
  [borders ^long w ^long h]
  (let [borders (fix-borders borders)]
    {:left (maybe-multiply (:left borders) w)
     :right (maybe-multiply (:right borders) w)
     :top (maybe-multiply (:top borders) h)
     :bottom (maybe-multiply (:bottom borders) h)}))

(def ^:private opposite-side? #{:right :bottom})

(defn calc-side
  "Add borders, side's sizes and gap.

  Return [total offset, side with added offset."
  [{:keys [inner outer ^long gap]} grid side]
  (let [^long post (if (opposite-side? side) (outer side) (inner side))
        ^long pre (if (opposite-side? side) (inner side) (outer side))
        [^long offset sides] (reduce (fn [[^long offset buff] s]
                                       (let [^long size (:size s)
                                             pre-offset (+ offset gap)]
                                         [(+ size pre-offset)
                                          (conj buff (assoc s :offset pre-offset))])) [pre []] (grid side))]
    (-> grid
        (update :offset assoc side (+ offset post gap))
        (assoc side sides))))


(defn- gather-side-sizes
  [grid selector id side]
  (let [ss (map side (vals (filter (comp #(and (vector? %)
                                               (= id (selector %))) first) grid))) ;; select sizes
        mcnt (reduce clojure.core/max (map count ss))] ;; maximum number of side boxes
    (map (fn [id]
           (reduce clojure.core/max (map #(:size (nth % id {:size 0})) ss))) (range mcnt)))) ;; find maximum for every box

(defn calc-inside
  [{:keys [rows cols view] :as grid}]
  (if (and rows (pos? rows)
           cols (pos? rows))
    (merge
     {:rows (into {} (for [row (range rows)]
                       [row {:top (gather-side-sizes grid first row :top)
                             :bottom (gather-side-sizes grid first row :bottom)}]))}
     {:columns (into {} (for [col (range cols)]
                          [col {:left (gather-side-sizes grid second col :left)
                                :right (gather-side-sizes grid second col :right)}]))})
    grid))

(defn calc-layout
  ([grid] (calc-layout grid {}))
  ([grid {:keys [^long x ^long y ^long w ^long h]
          :or {x 0 y 0 w 800 h 800}
          :as box}]
   (assert (and (pos? w) (pos? h)) (str "Too tight layout: " box))
   (let [borders (-> (or (:borders grid) default-borders)
                     (update :outer border-sizes w h)
                     (update :inner border-sizes w h))
         grid (reduce (partial calc-side borders) grid side-order)
         [^long l ^long r ^long t ^long b] (side-selector (:offset grid))]
     (-> grid
         (assoc
          :view {:x (+ x l)
                 :y (+ y t)
                 :w (- w l r)
                 :h (- h t b)}
          :box box)
         (calc-inside)))))

(calc-layout g)

;;

(require '[clojure2d.core :as c2d]
         '[clojure2d.extra.utils :as u]
         '[clojure2d.color :as c])

(def g (-> (grid 2 3)
           (add-side :left 32)
           (add-side :right {:size 111 :span? true})
           (add-side :right {:size 12 :span? true})
           (add-side :right 33)
           (add-side :top 11)
           (add-side :top {:size 22 :span? true})
           (add-side nil 0 :left 44)
           (add-side 0 nil :top 36)
           (add-side :bottom 1)
           (add-side :bottom 1)
           (add-side :bottom 100)
           (add-side :bottom 1)

           (add-side 1 1 :left 50)
           (add-side 0 1 :right 50)
           (add-side 1 2 :top 20)
           (add-side 0 0 :bottom 11)
           
           (update-border {:outer 0.02 :inner [15 120 110 23] :gap 4})
           ;; (calc-layout)
           ))

(g [0 0])

(def pal (c/palette 127))

(defn display-grid
  [{:keys [borders view left right top bottom]} box]
  (let [{:keys [x y w h]} view
        {:keys [gap outer inner]} borders
        [bl br bt bb] (-> (border-sizes outer (:w box) (:h box))
                          (side-selector))
        [ibl ibr ibt ibb] (-> (border-sizes inner (:w box) (:h box))
                              (side-selector))]
    (-> (c2d/with-canvas [c (c2d/canvas (:w box) (:h box))]
          (c2d/set-background c 0xfafafa)
          (c2d/set-color c (pal 0) 150)

          (doseq [{:keys [size offset]} left]
            (c2d/rect c (+ (:x box) offset) y size h))
          (doseq [{:keys [size offset]} right]
            (c2d/rect c (+ (:x box) w x offset) y size h))
          (doseq [{:keys [size offset span?]} top]
            (c2d/rect c
                      (if span? (+ bl gap) x)
                      (+ (:y box) offset)
                      (if span? (- (:w box) bl br gap gap) w)
                      size))
          (doseq [{:keys [size offset]} bottom]
            (c2d/rect c x (+ (:y box) h y offset) w size))
          (-> c
              (c2d/set-color :gray 70)
              (c2d/rect 0 0 bl (:h box))
              (c2d/rect 0 0 (:w box) bt)
              (c2d/rect 0 (- (:h box) bb) (:w box) bb)
              (c2d/rect (- (:w box) br) 0 br (:h box))

              (c2d/set-color :gray 70)
              
              (c2d/rect (- x ibl) (- y ibt) (+ w ibl ibr) (+ h ibt ibb))
              
              (c2d/set-color (pal 4))
              (c2d/rect x y w h))
          c)
        (c2d/save "layout.png")
        (u/show-image))))

(display-grid g {:x 0 :y 0 :w 800 :h 800})


;;

