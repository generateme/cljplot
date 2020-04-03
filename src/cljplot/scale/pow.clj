(ns cljplot.scale.pow
  (:require [fastmath.core :as m]
            [cljplot.scale.common :as sc]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn- symmetric
  [f]
  (fn ^double [^double x]
    (if (neg? x) (- ^double (f (- x))) (f x))))

(defn- cbrt ^double [^double x] (m/cbrt x))
(defn- sqrt ^double [^double x] (m/sqrt x))

(defn- pow-pairs
  [^double exponent]
  (case exponent
    m/THIRD [cbrt m/cb]
    0.5 [sqrt m/sq]
    1.0 [identity identity]
    2.0 [m/sq sqrt]
    3.0 [m/cb cbrt]
    (let [rexponent (/ exponent)]
      [(fn ^double [^double x] (m/pow x exponent))
       (fn ^double [^double x] (m/pow x rexponent))])))

(defn- pow-forward
  [pf ^double pstart ^double pend]
  (fn ^double [^double x]
    (m/norm (pf x) pstart pend)))

(defn- pow-inverse
  [pi ^double pstart ^double pend]
  (fn ^double [^double x]
    (pi (m/norm x 0.0 1.0 pstart pend))))

(defn pow
  "Power scale"
  ([] (pow [0.0 1.0]))
  ([domain] (pow domain 0.5))
  ([[start end :as domain] ^double exponent]
   (let [[pf pi] (map symmetric (pow-pairs exponent))
         pstart (pf start)
         pend (pf end)]
     (sc/->ContinuousScale start end domain :pow
                           (pow-forward pf pstart pend)
                           (pow-inverse pi pstart pend) {:exponent exponent}))))
