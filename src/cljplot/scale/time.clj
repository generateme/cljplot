(ns cljplot.scale.time
  (:require [fastmath.core :as m]
            [cljplot.scale.common :as sc]
            [java-time :as dt])
  (:import [java.time LocalDateTime]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:private epoch (dt/local-date-time 1970 1 1 0 0 0 0))

(defn- ld->ldt
  "Convert local-date to local-date-time"
  [ld]
  (cond
    (dt/local-date? ld) (dt/truncate-to (dt/local-date-time ld) :days)
    (dt/local-time? ld) (dt/local-date-time ld)
    (instance? java.util.Date ld) (->> (dt/instant ld)
                                       (dt/to-millis-from-epoch)
                                       (dt/millis)
                                       (dt/plus epoch))
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

(defn time-scale
  "Create time interval, works for any `java.time.temporal.Temporal` instance."
  [[start end]]
  (let [start (ld->ldt start)
        end (ld->ldt end)
        total (time-diff-millis start end)]
    (sc/->ContinuousScale start end [start end] :time (time-forward start total) (time-inverse start total) {:time-diff-millis total})))

;;;;;; ticks

(def ^:private ^:const ^long duration-second 1000)
(def ^:private ^:const ^long duration-minute (* duration-second 60))
(def ^:private ^:const ^long duration-hour (* duration-minute 60))
(def ^:private ^:const ^long duration-day (* duration-hour 24))
(def ^:private ^:const ^long duration-month (* duration-day 30))
(def ^:private ^:const ^long duration-year (* duration-day 365))

;; date time data set
(def ^:private dt-data
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
            :truncate identity}})

(defn- step->duration
  [^double step]
  (cond
    (> step duration-year) :years
    (> step duration-month) :months
    (> step duration-day) :days
    (> step duration-hour) :hours
    (> step duration-minute) :minutes
    (> step duration-second) :seconds
    :else :millis))

(defn- calc-dt-ticks
  "Calculate ticks"
  [start end ^double step]
  (let [{:keys [^double duration stepfn truncate]} (-> step
                                                       step->duration
                                                       dt-data)
        step (->> (/ step duration) m/floor stepfn)]
    (loop [s (dt/plus (truncate start) (stepfn 1))
           v []]
      (if (dt/before? s end)
        (recur (dt/plus s step) (if (dt/after? s start) (conj v s) v))
        v))))

(defn- diff->step
  ^double [^BigDecimal diff ^long ticks]
  (.doubleValue (.divide diff (BigDecimal. ticks) java.math.MathContext/DECIMAL128)))

(defn- time-ticks
  "Date time ticks."
  ([start end] (time-ticks start end 10))
  ([start end ^long ticks]
   (let [start (ld->ldt start)
         end (ld->ldt end)
         diff (time-diff-millis start end)
         step (diff->step diff ticks)]
     (calc-dt-ticks start end step))))

(defmethod sc/ticks :time [s & [c]]
  (time-ticks (:start s) (:end s) (or c 10)))

;; format

(defn- same-properties?
  "Compare given properties"
  [start end k]
  (let [prop (:property (dt-data k))]
    (= (dt/value (dt/property start prop))
       (dt/value (dt/property end prop)))))

(defn- format-dt-str
  "Infer format"
  [steps step]
  (let [s (first steps)
        e (last steps)
        same? (partial same-properties? s e)]
    (condp = (step->duration step)
      :years "y"
      :months (if-not (same? :years) "y-MM" "MMM")
      :days (if-not (same? :years)
              "y-MM-dd"
              (if-not (same? :months) "MMM-dd" "dd"))
      :hours (if-not (and (same? :years)
                          (same? :months)
                          (same? :days))
               "E HH:mm" "HH:mm")
      :minutes "HH:mm"
      :seconds (if-not (same? :minutes) "HH:mm:ss" "ss")
      :millis (if-not (same? :seconds) "ss.S" "S"))))

(defn time-format
  [scale ticks]
  (let [cnt (count ticks)
        step (diff->step (get-in scale [:info :time-diff-millis]) cnt)
        fmt (format-dt-str ticks step)]
    (if (<= cnt 1) str (partial dt/format fmt))))
