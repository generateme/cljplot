(ns cljplot.scale.log
  (:require [fastmath.core :as m]
            [cljplot.scale.linear :as sl]
            [cljplot.scale.common :as sc]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- log-forward
  [negative? ^double ls ^double le]
  (if negative?
    (fn ^double [^double v]
      (m/norm (m/ln (- v)) ls le))
    (fn ^double [^double v]
      (m/norm (m/ln v) ls le))))

(defn- log-inverse
  [negative? ^double ls ^double le]
  (if negative?
    (fn ^double [^double v]
      (- (m/exp (m/norm v 0.0 1.0 ls le))))
    (fn ^double [^double v]
      (m/exp (m/norm v 0.0 1.0 ls le)))))

(defn log
  "Log mapping function"
  ([] (log [1.0 10.0]))
  ([domain] (log domain 10.0))
  ([[^double start ^double end :as domain] base]
   (let [negative? (neg? start)
         ls (m/ln (if negative? (- start) start))
         le (m/ln (if negative? (- end) end))]
     (sc/->ContinuousScale start end domain :log
                           (log-forward negative? ls le)
                           (log-inverse negative? ls le) {:base base}))))

#_(defn log1p
    "Add 1 to the values"
    ([] (log1p [0.0 1.0]))
    ([domain] (log1p domain 10.0))
    ([[^double start ^double end :as domain] base]
     (let [scale (log [(inc start) (inc end)] base)
           f (:forward scale)
           i (:inverse scale)]
       (assoc scale
              :start start
              :end end
              :domain domain
              :type :log1p
              :forward (fn ^double [^double x] (f (inc x)))
              :inverse (fn ^double [^double x] (dec ^double (i x)))))))

;; ticks

(defn- logp 
  [^double base]
  (case base
    1.0 identity
    m/E (fn ^double [^double x] (m/log x))
    2.0 m/log2
    10.0 (fn ^double [^double x] (m/log10 x))
    (fn ^double [^double x] (m/logb base x))))

(defn- powp
  [^double base]
  (case base
    1.0 identity
    m/E (fn ^double [^double x] (m/exp x))
    (fn ^double [^double x] (m/pow base x))))

(defmethod sc/ticks :log [s & [c]]
  (let [base (get-in s [:info :base])
        ^double start (:start s)
        end (:end s)
        negative? (neg? start)
        logs (logp base)
        pows (powp base)
        ^double lstart (logs (m/abs start))
        ^double lend (logs (m/abs end))
        ^long c (or c (max 1 (- lend lstart)))
        accuracy (sc/tick-accuracy start end c)]
    (map (fn [^double v]
           (let [pv (-> v
                        pows
                        (m/approx accuracy)
                        (+ 0.0))]
             (if negative? (- pv) pv))) (sl/ticks-linear lstart lend c))))
