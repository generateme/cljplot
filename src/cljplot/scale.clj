(ns cljplot.scale
  (:require [fastmath.core :as m]
            [fastmath.interpolation :as i]
            [fastmath.stats :as s]
            [java-time :as dt])
  (:import [clojure.lang IFn]
           [java.time LocalDateTime]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defrecord ContinuousScale [start end domain type forward inverse info]
  IFn
  (invoke [_ v] (forward v))
  (invoke [_ s e v] (m/mlerp ^double s ^double e ^double (forward v))))

(defrecord DiscreteScale [start end domain range cnt type forward inverse info]
  IFn
  (invoke [_ v] (forward v))
  (invoke [_ xs v] (nth xs (forward v))))

(defrecord OrdinalScale [domain range type forward inverse info]
  IFn
  (invoke [_] range)
  (invoke [_ v] (forward v)))

(defn splice-range 
  "Splice range to get `cnt` number of points."
  ([^long cnt ^double start ^double end] (if (= cnt 1)
                                           (list (* 0.5 (- end start)))
                                           (map #(m/mnorm % 0.0 (dec cnt) start end) (range cnt))))
  ([^long cnt] (splice-range cnt 0.0 1.0)))

(defn- interpolated-range
  "Interpolate for ranges"
  [interpolator type xs]
  (let [target (splice-range (count xs))
        f (first xs)
        l (last xs)]
    (->ContinuousScale f l [f l] type (interpolator xs target) (interpolator target xs) nil)))

(defn linear
  "Linear mapping function"
  ([] (->ContinuousScale 0.0 1.0 [0.0 1.0] :linear identity identity nil))
  ([xs] (if-not (sequential? xs)
          (linear [0.0 xs])
          (condp #(== ^int %1 ^int %2) (count xs)
            0 (linear)
            1 (linear [0.0 (first xs)])
            2 (let [[start end] xs]
                (->ContinuousScale start end xs :linear
                                   (m/make-norm start end 0.0 1.0) (partial m/lerp start end) nil))
            (interpolated-range i/linear-smile :linear xs)))))

(defn spline
  "Spline (monotone) mapping function"
  ([] (linear))
  ([xs] (if-not (sequential? xs)
          (linear [0.0 xs])
          (if (<= (count xs) 2)
            (linear xs)
            (interpolated-range i/monotone :spline xs)))))

;;

(defn- log-forward
  "Logarithmic scale to [0,1]."
  [^double start ^double end]
  (let [v (/ (m/ln (/ end start)))]
    (fn [^double x] (* (m/ln (/ x start)) v))))

(defn- log-inverse
  "[0,1] to logarithmic scale"
  [^double start ^double end]
  (if (neg? start)
    (fn [^double t]
      (* (- (m/pow (- end) t))
         (m/pow (- start) (- 1.0 t))))
    (fn [^double t]
      (* (m/pow end t)
         (m/pow start (- 1.0 t))))))

(defn log
  "Log mapping function"
  ([] (log [1.0 10.0]))
  ([domain] (log domain 10.0))
  ([[start end :as domain] base] (->ContinuousScale start end domain :log (log-forward start end) (log-inverse start end) {:base base})))

(defn log1p
  "Add 1 to the values"
  ([] (log1p [1.0 10.0]))
  ([domain] (log1p domain 10.0))
  ([[^double start ^double end] base]
   (let [start (inc start)
         end (inc end)]
     (->ContinuousScale start end [start end] :log (comp (log-forward start end) #(inc ^double %)) (comp #(dec ^double %) (log-inverse start end)) {:base base}))))

;;

(defn- spow
  "Symmetric power (keeps sign)."
  ^double [^double x ^double p]
  (if (neg? x) (- (m/pow (- x) p)) (m/pow x p)))

(defn- pow-forward
  "Power scale to [0-1]"
  [^double start ^double end ^double exponent]
  (let [a (spow start exponent)
        v (/ (- (spow end exponent) a))]
    (fn [x] (* (- (spow x exponent) a) v))))

(defn- pow-inverse
  "Power scale inverse"
  [^double start ^double end ^double exponent]
  (let [a (spow start exponent)
        v (- (spow end exponent) a)
        re (/ 1.0 exponent)]
    (fn [^double t] (spow (+ a (* v t)) re))))

(defn pow
  "Power scale"
  ([] (pow [0.0 1.0]))
  ([domain] (pow domain 0.5))
  ([[start end :as domain] exponent] (->ContinuousScale start end domain :pow (pow-forward start end exponent) (pow-inverse start end exponent) {:exponent exponent})))

;; time for time!

(defn- ld->ldt
  "Convert local-date to local-date-time"
  [ld]
  (cond
    (dt/local-date? ld) (dt/truncate-to (dt/local-date-time ld) :days)
    (dt/local-time? ld) (dt/local-date-time ld)
    :else ld))

(defn- time-diff-millis
  "Calculate time duration in milliseconds.nanoseconds."
  ^BigDecimal [start end]
  (let [dur (dt/duration start end)
        seconds (BigDecimal. ^long (dt/value (dt/property dur :seconds)))
        nanos (.divide (BigDecimal. ^long (dt/value (dt/property dur :nanos))) 1000000.0M)]
    (.add nanos (.multiply seconds 1000.0M))))

(defn- time-forward
  "Create function which returns offset from starting date for given temporal value."
  [start ^BigDecimal total]
  (fn ^double [tm]
    (-> (time-diff-millis start (ld->ldt tm))
        (.divide total java.math.MathContext/DECIMAL128)
        (.doubleValue))))

(defn- time-inverse
  "Create function which returns date-time for given offset from start."
  [start ^BigDecimal total]
  (fn [^double t]
    (->> (BigDecimal. t)
         (.multiply total)
         (m/round)
         (dt/millis)
         (dt/plus start))))

(defn time-interval
  "Create time interval, works for any `java.time.temporal.Temporal` instance."
  [[start end]]
  (let [start (ld->ldt start)
        end (ld->ldt end)
        total (time-diff-millis start end)]
    (->ContinuousScale start end [start end] :time (time-forward start total) (time-inverse start total) {:time-diff-millis total})))

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
    {:years {:property :year
             :duration duration-year
             :stepfn dt/years
             :truncate #(dt/truncate-to (dt/adjust % :first-day-of-year) :days)}
     :months {:property :month-of-year
              :duration duration-month
              :stepfn dt/months
              :truncate #(dt/truncate-to (dt/adjust % :first-day-of-month) :days)}
     :days {:property :day-of-month
            :duration duration-day
            :stepfn dt/days
            :truncate #(dt/truncate-to % :days)}
     :hours {:property :hour-of-day
             :duration duration-hour
             :stepfn dt/hours
             :truncate #(-> ^LocalDateTime %
                            (.withNano 0)
                            (.withMinute 0)
                            (.withSecond 0))}
     :minutes {:property :minute-of-hour
               :duration duration-minute
               :stepfn dt/minutes
               :truncate #(-> ^LocalDateTime %
                              (.withNano 0)
                              (.withSecond 0))}
     :seconds {:property :second-of-minute
               :duration duration-second
               :stepfn dt/seconds
               :truncate #(-> ^LocalDateTime %
                              (.withNano 0))}
     :millis {:property :millis-of-second
              :duration 1
              :stepfn dt/millis
              :truncate identity}}))

(defn- same-properties?
  "Compare given properties"
  [start end k]
  (let [prop (:property (dt-data k))]
    (= (dt/value (dt/property start prop))
       (dt/value (dt/property end prop)))))

(defn- format-dt-str
  "Infer format"
  [steps k]
  (let [s (first steps)
        e (last steps)
        same? (partial same-properties? s e)]
    (cond
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
      (= :millis k) (if-not (same? :seconds) "ss.S" "S"))))

(defn- format-dt
  [steps k]
  (if (<= (count steps) 1) str (partial dt/format (format-dt-str steps k))))

(defn- calc-dt-ticks
  "Calculate ticks"
  [start end ^double step k]
  (let [{:keys [^double duration stepfn truncate]} (dt-data k)
        step (stepfn (m/ceil (/ step duration)))
        steps (loop [s (truncate start)
                     v []]
                (if (dt/before? s end)
                  (recur (dt/plus s step) (if (dt/after? s start) (conj v s) v))
                  v))]
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
   (let [start (ld->ldt start)
         end (ld->ldt end)
         diff (time-diff-millis start end)
         step (.doubleValue (.divide diff (BigDecimal. ticks) java.math.MathContext/DECIMAL128))]
     (infer-dt-ticks start end step))))

;; continuous->discrete

(defn- interval-steps-before
  "Maps `steps` values into ordinal values (0,1,2...)."
  [steps]
  (comp int (i/step-before steps (range (count steps)))))

(defn quantile
  "Returns discrete range for evenly distributed quantiles."
  ([xs] (quantile xs 10))
  ([xs steps-no] (quantile xs steps-no :legacy))
  ([xs ^long steps-no estimation-strategy]
   (let [xs (m/seq->double-array xs)
         [mn mx] (s/extent xs)
         steps (s/quantiles xs (rest (splice-range (inc steps-no))) estimation-strategy)]
     (->DiscreteScale mn mx steps-no :quantile (interval-steps-before steps) nil {:quantiles steps}))))

(defn- threshold-from-steps
  [steps]
  (if-not (sequential? steps)
    (threshold-from-steps (splice-range (inc ^long steps)))
    (let [[mn mx] (s/extent steps)]
      (->DiscreteScale mn mx (dec (count steps)) :threshold (interval-steps-before (rest steps)) nil {:steps steps}))))

(defn threshold
  "Returns discrete range for given steps or evenly spliced range."
  ([steps] (threshold-from-steps steps))
  ([[start end] ^long steps-no] (threshold-from-steps (splice-range (inc steps-no) start end))))

;; ordinal

(defn ordinal
  ""
  ([xs] (ordinal xs :ordinal))
  ([xs type] (ordinal xs type nil))
  ([xs type info]
   (let [d (range (count xs))
         r (vec xs)]
     (->OrdinalScale d r type r (zipmap xs d) info))))

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
  ([b {:keys [^double padding-in ^double padding-out ^double align]
       :or {padding-in 0.0 padding-out 0.0 align 0.5}}]
   (let [[bands-no bands] (if (sequential? b)
                            [(count b) b]
                            [b (range b)])
         bands-no (int bands-no)
         padding-in (m/constrain ^double padding-in 0.0 1.0)
         align (m/constrain ^double align 0.0 1.0)
         step (/ (+ (* bands-no (- 1.0 padding-in))
                    (+ padding-out padding-out)
                    (* (dec bands-no) padding-in)))
         nstart (* step padding-out)
         size (* step (- 1.0 padding-in))
         lst (for [^long i (range bands-no)
                   :let [lstart (+ nstart (* i step))
                         lend (+ lstart size)
                         [lstart lend] (if (neg? step) [lend lstart] [lstart lend])]]
               {:start lstart
                :end lend
                :point (m/lerp lstart lend align)})]
     (->OrdinalScale bands lst :bands
                     (zipmap bands lst)
                     (bands-inverse-fn bands lst) {:bandwidth (m/abs size)
                                                   :step (m/abs step)}))))

;; inverse

(defn inverse
  "Return inverse scaling function [0,1]->domain value. If not available, return `nil`."
  [s v]
  (when-let [inv (:inverse s)] (inv v)))

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
          [^double start ^double stop] (if reverse? [stop start] [start stop])
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

(defmethod ticks- :default [s c]
  {:ticks (map #(+ 0.0 (m/approx % 4)) (ticks-linear (:start s) (:end s) (or c 10)))})

(defn- logp 
  ""
  [^double base]
  (condp #(== ^double %1 ^double %2) base
    m/E (fn [x] (m/log x))
    2 (fn [x] (m/log2 x))
    10 (fn [x] (m/log10 x))
    (fn [x] (m/logb base x))))

(defn- powp
  ""
  [base]
  (condp #(== ^double %1 ^double %2) base
    m/E (fn [x] (m/exp x))
    (fn [x] (m/pow base x))))

(defmethod ticks- :log [s & [c]]
  (let [^double c (or c 10.0)
        base (:base (:info s))
        logs (logp base)
        pows (powp base)
        ^double start (logs (:start s))
        ^double end (logs (:end s))
        lst (map (comp m/approx pows) (ticks-linear start end (min (- end start) c)))]
    {:ticks (map #(+ 0.0 ^double %) (if (seq lst)
                                      lst
                                      [(m/approx (pows start))]))}))

(defmethod ticks- :bands [s c]
  {:ticks (if c
            (take-nth (max 1 (int (m/floor (/ ^int (count (:domain s)) (double c))))) (:domain s))
            (:domain s))})

(defmethod ticks- :time [s c] (ticks-dt (:start s) (:end s) (or c 10.0)))

;; scale/ticks factory

(def ^:private scale-names
  {:linear linear
   :log log
   :log1p log1p
   :pow pow
   :time time-interval
   :bands bands
   :ordinal ordinal
   :quantile quantile
   :threshold threshold})

(def ^:private default-domain
  {:linear [0 1]
   :pow [0 1]
   :log [1.0 10.0]
   :log1p [0.0 9.0]
   :time [(dt/local-date-time) (dt/plus (dt/local-date-time) (dt/years 1))]
   :bands 1
   :ordinal [0]
   :quantile [0]
   :threshold 10})

(defn- make-scale
  ([scale-def] (make-scale scale-def nil))
  ([[scale-name & r] domain]
   (let [domain (or domain (default-domain scale-name))
         r (conj r domain)]
     {:scale (apply (scale-names scale-name) r)
      :domain domain})))

(defn- make-ticks
  ([scale] (make-ticks scale nil))
  ([scale ticks]
   (if-not (sequential? ticks)
     (ticks- scale ticks)
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


