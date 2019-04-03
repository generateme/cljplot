(ns cljplot.scale
  (:require [fastmath.core :as m]
            [fastmath.interpolation :as i]
            [fastmath.stats :as s]
            [java-time :as dt]
            [clojure2d.color :as c])
  (:import [clojure.lang IFn]))

;; continuous -> continuous

(defrecord ContinuousRange [start end type forward inverse info]
  IFn
  (invoke [_ v] (forward v))
  (invoke [_ s e v] (m/mlerp s e (forward v))))

(defrecord DiscreteRange [start end cnt type forward inverse info]
  IFn
  (invoke [_ v] (forward v))
  (invoke [_ xs v] (nth xs (forward v))))

(defrecord OrdinalRange [domain range type forward inverse info]
  IFn
  (invoke [_] range)
  (invoke [_ v] (forward v))
  (invoke [_ s e v] (m/mlerp s e ((:value info) (forward v)))))

(defn splice-range 
  "Splice range to get `cnt` number of points."
  ([cnt start end] (map #(m/norm % 0.0 (dec cnt) start end) (range cnt)))
  ([cnt] (splice-range cnt 0.0 1.0)))

(defn- interpolated-range
  "Interpolate for ranges"
  [interpolator type xs]
  (let [target (splice-range (count xs))]
    (->ContinuousRange (first xs) (last xs) type (interpolator xs target) (interpolator target xs) nil)))

(defn linear
  "Linear mapping function"
  ([] (->ContinuousRange 0.0 1.0 :linear identity identity nil))
  ([xs] (if-not (sequential? xs)
          (linear [0.0 xs])
          (condp == (count xs)
            0 (linear)
            1 (linear [0.0 (first xs)])
            2 (let [[start end] xs]
                (->ContinuousRange start end :linear
                                   (m/make-norm start end 0.0 1.0) (partial m/lerp start end) nil))
            (interpolated-range i/linear-smile :linear xs)))))

(defn spline
  "Spline mapping function"
  ([] (linear))
  ([xs] (if-not (sequential? xs)
          (linear [0.0 xs])
          (if (<= (count xs) 2)
            (linear xs)
            (interpolated-range i/cubic-spline :spline xs)))))

;;

(defn- log-forward
  "Logarithmic scale to [0,1]."
  [start end]
  (let [v (/ (m/ln (/ end start)))]
    (fn [x] (* (m/ln (/ x start)) v))))

(defn- log-inverse
  "[0,1] to logarithmic scale"
  [start end]
  (if (neg? start)
    (fn [t]
      (* (- (m/pow (- end) t))
         (m/pow (- start) (- 1.0 t))))
    (fn [t]
      (* (m/pow end t)
         (m/pow start (- 1.0 t))))))

(defn log
  "Log mapping function"
  ([] (log 10.0 [1.0 10.0]))
  ([domain] (log 10.0 domain))
  ([base [start end]] (->ContinuousRange start end :log (log-forward start end) (log-inverse start end) {:base base})))

(defn log1p
  "Add 1 to the values"
  ([] (log1p 10.0 [1.0 10.0]))
  ([domain] (log1p 10.0 domain))
  ([base [start end]]
   (let [start (inc start)
         end (inc end)]
     (->ContinuousRange start end :log (comp (log-forward start end) inc) (comp dec (log-inverse start end)) {:base base}))))

;;

(defn- spow
  "Symmetric power (keeps sign)."
  [x p]
  (if (neg? x) (- (m/pow (- x) p)) (m/pow x p)))

(defn- pow-forward
  "Power scale to [0-1]"
  [start end exponent]
  (let [a (spow start exponent)
        v (/ (- (spow end exponent) a))]
    (fn [x] (* (- (spow x exponent) a) v))))

(defn- pow-inverse
  "Power scale inverse"
  [start end exponent]
  (let [a (spow start exponent)
        v (- (spow end exponent) a)
        re (/ 1.0 exponent)]
    (fn [t] (spow (+ a (* v t)) re))))

(defn pow
  "Power scale"
  ([] (pow 0.5 [0.0 1.0]))
  ([domain] (pow 0.5 domain))
  ([exponent [start end]] (->ContinuousRange start end :pow (pow-forward start end exponent) (pow-inverse start end exponent) {:exponent exponent})))

;;

(defn- ld->ldt
  "Convert local-date to local-date-time"
  [ld]
  (cond
    (dt/local-date? ld) (dt/truncate-to (dt/local-date-time ld) :days)
    (dt/local-time ld) (dt/local-date-time ld)
    :else ld))

(defn- time-diff-millis
  "Calculate time duration in milliseconds.nanoseconds."
  ^double [start end]
  (let [dur (dt/duration (ld->ldt start) (ld->ldt end))
        seconds (dt/value (dt/property dur :seconds))
        nanos (/ (dt/value (dt/property dur :nanos)) 1000000.0)]
    (double (+ nanos (* 1000 seconds)))))

(defn- time-forward
  "Create function which returns offset from starting date for given temporal value."
  [start total]
  (fn [tm]
    (/ (time-diff-millis start tm)
       total)))

(defn- time-inverse
  "Create function which returns date-time for given offset from start."
  [start total]
  (fn [t]
    (->> t
         (* total)
         (m/round)
         (dt/millis)
         (dt/plus start))))

(defn time-interval
  "Create time interval, works for any `java.time.temporal.Temporal` instance."
  [[start end]]
  (let [total (time-diff-millis start end)]
    (->ContinuousRange start end :time (time-forward (dt/local-date-time start) total) (time-inverse (dt/local-date-time start) total) {:time-diff-millis total})))

(defn- truncate-ym
  "Create truncating function for year and month"
  [f pos]
  (let [step (f 1)]
    (fn [d]
      (dt/truncate-to (dt/plus (dt/adjust d pos) step) :days))))

;; How much millis per each duration
(def ^:private ^:const ^long duration-second 1000)
(def ^:private ^:const ^long duration-minute (* duration-second 60))
(def ^:private ^:const ^long duration-hour (* duration-minute 60))
(def ^:private ^:const ^long duration-day (* duration-hour 24))
(def ^:private ^:const ^long duration-month (* duration-day 30))
(def ^:private ^:const ^long duration-year (* duration-day 365))

;; date time data set
(def ^:private dt-data
  (let [d1 (dt/days 1)
        h1 (dt/hours 1)
        m1 (dt/minutes 1)
        s1 (dt/seconds 1)
        ml1 (dt/millis 1)]
    {:years {:property :year :duration duration-year :stepfn dt/years :round (truncate-ym dt/years :first-day-of-year)}
     :months {:property :month-of-year :duration duration-month :stepfn dt/months :round (truncate-ym dt/months :first-day-of-month)}
     :days {:property :day-of-month :duration duration-day :stepfn dt/days :round #(dt/truncate-to (dt/plus % d1) :days)}
     :hours {:property :hour-of-day :duration duration-hour :stepfn dt/hours :round #(-> (dt/plus % h1)
                                                                                         (.withNano 0)
                                                                                         (.withMinute 0)
                                                                                         (.withSecond 0))}
     :minutes {:property :minute-of-hour :duration duration-minute :stepfn dt/minutes :round #(-> (dt/plus % m1)
                                                                                                  (.withNano 0)
                                                                                                  (.withSecond 0))}
     :seconds {:property :second-of-minute :duration duration-second :stepfn dt/seconds :round #(-> (dt/plus % s1)
                                                                                                    (.withNano 0))}
     :millis {:property :millis-of-second :duration 1 :stepfn dt/millis :round #(dt/plus % ml1)}}))

(defn- same-properties?
  "Compare given properties"
  [start end k]
  (let [prop (:property (dt-data k))]
    (== (dt/value (dt/property start prop))
        (dt/value (dt/property end prop)))))

(defn- format-dt
  "Infer format"
  [steps k]
  (if (<= (count steps) 1) str
      (let [s (first steps)
            e (last steps)
            same? (partial same-properties? s e)]
        (partial dt/format (cond
                             (= :years k) "y"
                             (= :months k) (if-not (same? :years) "y-MM" "MMM")
                             (= :days k) (if-not (same? :years)
                                           "y-MM-dd" (if-not (same? :months) "MMM-dd" "dd"))
                             (= :hours k) (if-not (and (same? :years)
                                                       (same? :months)
                                                       (same? :days))
                                            "E HH:mm" "HH:mm")
                             (= :minutes k) "HH:mm"
                             (= :seconds k) (if-not (same? :minutes) "HH:mm:ss" "ss")
                             (= :millis k) (if-not (same? :seconds) "ss.S" "S"))))))


(defn- calc-dt-ticks
  "Calculate ticks"
  [start end step k]
  (let [start (ld->ldt start)
        end (ld->ldt end)
        {:keys [duration stepfn round]} (dt-data k)
        step (stepfn (m/ceil (/ step duration)))
        steps (loop [s (round start)
                     v [s]]
                (let [ns (dt/plus s step)]
                  (if (dt/before? ns end)
                    (recur ns (conj v ns))
                    v)))]
    {:ticks steps
     :fmt (format-dt steps k)}))

(defn- infer-dt-ticks
  "Inter ticks from step size."
  [start end ^double step]
  (let [f (partial calc-dt-ticks start end step)]
    (cond
      (> step duration-year) (f :years)
      (> step duration-month) (f :months)
      (> step duration-day) (f :days)
      (> step duration-hour) (f :hours)
      (> step duration-minute) (f :minutes)
      (> step duration-second) (f :seconds)
      :else (f :millis))))

(defn- ticks-dt
  "Date time ticks."
  ([start end] (ticks-dt start end 10))
  ([start end ^long ticks]
   (let [diff (time-diff-millis start end)
         step (/ diff ticks)]
     (infer-dt-ticks start end step))))

;; continuous->discrete

(defn- interval-steps-before
  "Maps `steps` values into ordinal values (0,1,2...)."
  [steps]
  (comp int (i/step-before steps (range (count steps)))))

(defn- interval-steps-after
  "Maps `steps` values into ordinal values (0,1,2...)."
  [steps]
  (comp int (i/step-after steps (range (count steps)))))

(defn quantile
  ""
  ([xs] (quantile 10 xs))
  ([steps-no xs] (quantile steps-no :legacy xs))
  ([steps-no estimation-strategy xs]
   (let [xs (m/seq->double-array xs)
         [mn mx] (s/extent xs)
         steps (map #(s/quantile xs % estimation-strategy)
                    (rest (splice-range (inc steps-no))))]
     (->DiscreteRange mn mx steps-no :quantile (interval-steps-before steps) nil {:quantiles steps}))))

(defn- threshold-from-steps
  [steps]
  (if-not (sequential? steps)
    (threshold-from-steps (splice-range (inc steps)))
    (let [[mn mx] (s/extent steps)]
      (->DiscreteRange mn mx (dec (count steps)) :threshold (interval-steps-before (rest steps)) nil {:steps steps}))))

(defn threshold
  ""
  ([steps-no [start end]] (threshold-from-steps (splice-range (inc steps-no) start end)))
  ([steps] (threshold-from-steps steps)))

;; ordinal

(defn ordinal
  ""
  ([xs] (ordinal :ordinal xs))
  ([type xs] (ordinal type nil xs))
  ([type info xs]
   (let [d (range (count xs))
         r (vec xs)]
     (->OrdinalRange d r type r (zipmap xs d) info))))

(defn- bands-inverse-fn
  "Inverse function for bands."
  [bands lst]
  (fn [^double v]
    (loop [s (map vector bands lst)]
      (when (seq s)
        (let [[band-id {:keys [^double start ^double end]}] (first s)]
          (if (<= start v end) band-id (recur (next s))))))))

(defn bands
  "Creates sequence of bands for given range and padding.

  Bands are evenly distributed intervals with padding.

  Each band is a map with following keys:

  * start - interval start
  * end - interval end
  * point - selected point (default: midpoint)

  Input parameters are:

  * bands - number of the bands (default: 1) or sequence of values
  * padding-in - padding between bands (default: 0.0)
  * padding-out - border padding (default: 0.0)
  * align - position of the selected point (0.0 - left, 1.0 - right, 0.5 - midpoint, default)

  Padding is calculated the same way as in `d3`. It's a proportion of the step."
  ([] (bands 1))
  ([b] (bands {} b))
  ([{:keys [^double padding-in ^double padding-out ^double align]
     :or {padding-in 0.0 padding-out 0.0 align 0.5}} bands]
   (let [[bands-no bands] (if (sequential? bands)
                            [(count bands) bands]
                            [bands (range bands)])
         padding-in (m/constrain ^double padding-in 0.0 1.0)
         align (m/constrain ^double align 0.0 1.0)
         step (/ (+ (* bands-no (- 1.0 padding-in))
                    (+ padding-out padding-out)
                    (* (dec bands-no) padding-in)))
         nstart (* step padding-out)
         size (* step (- 1.0 padding-in))
         lst (for [i (range bands-no)
                   :let [lstart (+ nstart (* i step))
                         lend (+ lstart size)
                         [lstart lend] (if (neg? step) [lend lstart] [lstart lend])]]
               {:start lstart
                :end lend
                :point (m/mlerp lstart lend align)})]
     (->OrdinalRange bands lst :bands
                     (zipmap bands lst)
                     (bands-inverse-fn bands lst) {:bandwidth (m/abs size)
                                                   :step (m/abs step)
                                                   :value :point}))))

;; inverse

(defn inverse
  "Return inverse scaling function [0,1]->domain value. If not available, return `nil`."
  [s v]
  (when-let [inv (:inverse s)]
    (inv v)))

;;

;; ticks
;; d3 style

(def ^:const ^:private ^double e10 (m/sqrt 50.0))
(def ^:const ^:private ^double e5 (m/sqrt 10.0))
(def ^:const ^:private ^double e2 m/SQRT2)

(defn- step-mult
  ""
  ^double [^double error]
  (cond
    (>= error e10) 10.0
    (>= error e5) 5.0
    (>= error e2) 2.0
    :else 1.0))

(defn- tick-step
  ""
  ^double [^double start ^double stop ^long count]
  (let [step0 (/ (m/abs (- stop start)) (max 0 count))
        step1 (m/pow 10 (m/floor (m/log10 step0)))
        error (/ step0 step1)
        ret (* step1 (step-mult error))]
    (if (< stop start) (- ret) ret)))

(defn- tick-increment
  ""
  ^double [^double start ^double stop ^long count]
  (let [step (/ (- stop start) (max 0 count))
        power (m/floor (m/log10 step))
        p10 (m/pow 10.0 power)
        error (/ step p10)]
    (if (>= power 0.0)
      (* p10 (step-mult error))
      (/ (- (m/pow 10.0 (- power))) (step-mult error)))))

(defn- ticks-linear
  ""
  [^double start ^double stop ^long count]
  (if (and (== start stop) (pos? count))
    [start]
    (let [reverse? (< stop start)
          [start stop] (if reverse? [stop start] [start stop])
          step (tick-increment start stop count)
          res (cond
                (or (zero? step)
                    (Double/isInfinite step)) []
                (pos? step) (let [start (m/ceil (/ start step))
                                  stop (m/floor (/ stop step))
                                  n (m/ceil (inc (- stop start)))]
                              (map #(* (+ start ^double %) step) (range n)))
                :else (let [start (m/floor (* start step))
                            stop (m/ceil (* stop step))
                            n (m/ceil (inc (- start stop)))]
                        (map #(/ (- start ^double %) step) (range n))))]
      (if reverse? (reverse res) res))))

(defmulti ticks- (fn [s & _] (:type s)))

(defmethod ticks- :default [s & [c]]
  {:ticks (map #(+ 0.0 (m/approx % 4)) (ticks-linear (:start s) (:end s) (or c 10)))})

(defn- logp 
  ""
  [base]
  (condp == base
    m/E (fn [x] (m/log x))
    2 (fn [x] (m/log2 x))
    10 (fn [x] (m/log10 x))
    (fn [x] (m/logb base x))))

(defn- powp
  ""
  [base]
  (condp == base
    m/E (fn [x] (m/exp x))
    (fn [x] (m/pow base x))))

(defmethod ticks- :log [s & [c]]
  (let [c (or c 10)
        base (:base (:info s))
        logs (logp base)
        pows (powp base)
        start (logs (:start s))
        end (logs (:end s))
        lst (map (comp m/approx pows) (ticks-linear start end (min (- end start) c)))]
    {:ticks (map #(+ 0.0 %) (if (seq lst)
                              lst
                              [(m/approx (pows start))]))}))

(defmethod ticks- :bands [s & _] {:ticks (:domain s)})
(defmethod ticks- :time [s & [c]] (ticks-dt (:start s) (:end s) (or c 10)))

(comment defn ticks-log
         ""
         [start stop count base logs pows]
         (let [reverse? (< start stop)
               [u v] (if reverse? [stop start] [start stop])
               i (logs u)
               j (logs v)
               n (or count 10)]
           (if (and (zero? (m/frac base))
                    (< n (- j i)))
             (let [i (dec (m/round i))
                   j (inc (m/round j))]
               (if (pos? u)
                 (for [ii (range i j)]))))))


;; scale/ticks factory

(def ^:private scale-names
  {:time time-interval
   :linear linear
   :log log
   :log1p log1p
   :pow pow
   :bands bands
   :ordinal ordinal
   :threshold threshold})

(def ^:private default-domain
  {:log [1.0 10.0]
   :log1p [0.0 9.0]
   :time [(dt/local-date-time) (dt/plus (dt/local-date-time) (dt/years 1))]
   :bands 1
   :ordinal [0]
   :threshold 10})

(defn- make-scale
  ([scale-def] (make-scale scale-def nil))
  ([[scale-name & r] domain]
   (let [domain (or domain (get default-domain scale-name [0.0 1.0]))
         r (conj (vec r) domain)]
     {:scale (apply (scale-names scale-name) r)
      :domain domain})))

(defn- make-ticks
  ([scale] (make-ticks scale nil))
  ([scale ticks]
   (if-not (sequential? ticks)
     (ticks- scale (max 1 (long (or ticks 10))))
     {:ticks ticks})))

(defn- coerce-format-fn
  "Find formating funciton."
  ([fmt] (coerce-format-fn nil fmt))
  ([scale-type fmt]
   (cond
     (string? fmt) (partial (if (= scale-type :time) dt/format format) fmt)
     (fn? fmt) fmt
     :else str)))

(defn scale-map
  "Create scale data from definition"
  ([scale-def] (scale-map scale-def nil))
  ([scale-def {tick-values :ticks
               domain :domain
               ofmt :fmt}]
   (let [{:keys [domain scale]} (make-scale scale-def domain)
         {:keys [ticks fmt]} (make-ticks scale tick-values)]
     {:domain domain
      :fmt (coerce-format-fn (:type scale) (or ofmt fmt))
      :ticks ticks
      :scale scale
      :scale-def scale-def})))

(defn- scale-type-from-map
  [scale-map]
  (:type (:scale scale-map)))

(defn update-scale
  [sc field value]
  (case field
    :domain (-> (scale-map (:scale-def sc) {:domain value})
               (assoc :fmt (:fmt sc))) ;; recalculate everything, keep format
    :fmt (assoc sc :fmt (coerce-format-fn (scale-type-from-map sc) value))
    :scale (scale-map value (dissoc sc :ticks)) ;; recalculate ticks
    :ticks (merge sc (make-ticks (:scale sc) value))
    sc))
