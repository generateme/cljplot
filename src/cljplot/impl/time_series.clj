(ns cljplot.impl.time-series
  (:require [clojure2d.core :as c2d]
            [cljplot.common :as common]
            [fastmath.stats :as stats]
            [fastmath.core :as m]
            [fastmath.random :as rnd]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defmethod common/prepare-data :lag [_ data {:keys [lag]}] (map vector data (drop (or lag 1) data)))
(defmethod common/render-graph :lag [_ data conf chart-data] (common/render-graph :scatter data conf chart-data))

(defn- ensure-vec
  [data]
  (if (vector? data) data (vec data)))

;; TODO move to fastmath

(defn acf
  "Calculate acf for given number of lags"
  [data ^long lags]
  (let [vdata (ensure-vec data)]
    (mapv (fn [lag]
            (let [v2 (subvec vdata lag)
                  v1 (subvec vdata 0 (count v2))]
              (stats/correlation v1 v2))) (range (inc lags)))))

;; http://feldman.faculty.pstat.ucsb.edu/174-03/lectures/l13
(defn pacf
  [data ^long lags]
  (let [acf (acf data lags)
        phis (reductions (fn [curr ^long id]
                           (let [phi (/ (- ^double (acf id) ^double (reduce m/+ (map-indexed #(* ^double (acf (dec (- id ^int %1))) ^double %2) curr)))
                                        (- 1.0 ^double (reduce m/+ (map-indexed #(* ^double (acf (inc ^int %1)) ^double %2) curr))))]
                             (conj (mapv #(- ^double %1 (* phi ^double %2)) curr (reverse curr)) phi))) [(acf 1)] (range 2 (inc lags)))]
    (vec (conj (map last phis) 0.0))))

(defn- p-acf-data
  [method data lags]
  (let [vdata (ensure-vec data)
        cnt (count vdata)
        lags (or lags (min (dec cnt) (m/ceil (* 10.0 (m/log10 cnt)))))
        acf-fn (if (= method :acf) acf pacf)
        acf (map-indexed vector (acf-fn vdata lags))
        rsqrt (/ 1.0 (m/sqrt cnt))
        ci (* rsqrt ^double (rnd/icdf rnd/default-normal (* 0.5 (inc 0.95))))]
    (if (= method :acf)
      [acf rsqrt ci (mapv #(* ci (m/sqrt (dec (* 2.0 ^double %))))
                          (reductions (fn [^double acc ^double s] (+ acc (* s s))) (map second acf)))]
      [(rest acf) rsqrt ci])))

#_(last (p-acf-data :acf ma2 10))

;; TODO move to test

#_(def ep (repeatedly 10000 rnd/grand))
#_(def ma2 (take 500 (map (fn [e1 e2 e3] (+ (- e1) (* 0.8 e2) (* -0.9 e3))) (drop 2 ep) (drop 1 ep) ep)))

#_ (mapv second (rest (first (p-acf-data :acf ma2 5))))
;; => (-0.5860175473046139 0.28776310296448715 0.03196383068903399 0.007866082297411425 -0.08087093130708278)


#_(let [rho (mapv second (first (p-acf-data :acf ma2 5)))
        rho1 (rho 1)
        rho2 (rho 2)
        phi11 rho1
        phi22 (/ (- rho2 (* phi11 rho1))
                 (- 1.0 (* phi11 rho1)))
        phi21 (- phi11 (* phi22 phi11))
        rho3 (rho 3)
        phi33 (/ (- rho3 (* phi21 rho2) (* phi22 rho1))
                 (- 1.0 (* phi21 rho1) (* phi22 rho2)))
        phi31 (- phi21 (* phi33 phi22))
        phi32 (- phi22 (* phi33 phi21))
        rho4 (rho 4)
        phi44 (/ (- rho4 (* phi31 rho3) (* phi32 rho2) (* phi33 rho1))
                 (- 1.0 (* phi31 rho1) (* phi32 rho2) (* phi33 rho3)))]
    [phi22 phi33 phi44])
;; => [-0.08476220976837147 0.25345653039561855 0.25255420956957964]


#_ (mapv second (first (p-acf-data :pacf ma2 5)))


(defmethod common/prepare-data :acf [_ data {:keys [lags]}] (p-acf-data :acf data lags))

(defmethod common/data-extent :acf [_ data _]
  (let [e (common/common-extent (first data))
        [_ [^double mn mx]] (:y e)]
    (assoc-in e [:y 1] [(min 0.0 mn) mx])))

;; pars: ci 0.95, color :red
(defmethod common/render-graph :acf [_ [data ^double _rsqrt ^double ci0 cis] _conf {:keys [^int w ^int h x y] :as chart-data}]
  (let [scale-x (partial (:scale x) 0 w)
        scale-y (partial (:scale y) 0 h)
        zero (scale-y 0.0)
        w- (dec w)
        ^double ci- (scale-y (- ci0))
        ^double ci (scale-y ci0)]
    (common/do-graph (assoc chart-data :oversize 0) false

      (c2d/set-color c :grey 80)

      (if cis
        (let [p1 (map-indexed #(vector (scale-x %1) (scale-y %2)) cis)
              p2 (reverse (map-indexed #(vector (scale-x %1) (scale-y (- ^double %2))) cis))] 
          (c2d/path c (concat p2 p1) true false))
        
        (c2d/rect c 0 ci- w (- ci ci-)))
      
      (-> c
          (c2d/set-color :black)
          (c2d/line 0 zero w- zero)) 
      (doseq [[id [^double x ^double y]] (map-indexed vector data)
              :let [xx (scale-x x)
                    yy (scale-y y)]]
        (c2d/set-color c :black)
        (c2d/line c xx zero xx yy)
        (when (or (and (not cis) (> (m/abs y) ci0))
                  (and cis (> (m/abs y) ^double (cis id))))
          (c2d/set-color c :red))
        (c2d/ellipse c xx yy 4 4)))))

;; pacf

(defmethod common/prepare-data :pacf [_ data {:keys [lags]}] (p-acf-data :pacf data lags))
(defmethod common/data-extent :pacf [_ data conf] (common/data-extent :acf data conf))
(defmethod common/render-graph :pacf [_ data conf chart-data] (common/render-graph :acf data conf chart-data))

(m/unuse-primitive-operators)
