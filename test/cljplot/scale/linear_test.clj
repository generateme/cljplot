(ns cljplot.scale.linear-test
  (:require [cljplot.scale.linear :refer [linear ticks-linear]]
            [cljplot.scale :refer [inverse]]
            [clojure.test :refer [deftest is]]))

(deftest linear-basic-test
  (let [l (linear)]
    (is (= 0.0 (:start l)))
    (is (= 1.0 (:end l)))
    (is (= [0.0 1.0] (:domain l)))
    (is (= :linear (:type l)))
    (is (= 0.5 (l 0.5)))
    (is (= 10.0 (l 0 20 0.5)))
    (is (= -1.0 (l -1.0)))
    (is (= -20.0 (l 0 20 -1.0)))
    (is (= 0.5 (inverse l 0.5)))))

(deftest linear-mapping-test
  (let [l (linear [3 4])]
    (is (= 0.5 (l 3.5)))
    (is (= 3.5 (inverse l 0.5)))
    (is (= 1.5 (l 1 2 3.5)))))

(deftest linear-simple-domain-test
  (let [l1 (linear [4.0])
        l2 (linear nil)
        l3 (linear 8.0)]
    (is (= [0.0 4.0] (:domain l1)))
    (is (= [0.0 1.0] (:domain l2)))
    (is (= [0.0 8.0] (:domain l3)))))

(deftest linear-multilinear-test
  (let [l (linear [-10.0 0.0 100.0])]
    (is (= [-10.0 100.0] (:domain l)))
    (is (= [-10.0 0.0 100.0] (get-in l [:info :steps])))
    (is (= 0.5 (l 0.0)))
    (is (= 0.0 (l -10.0)))
    (is (= 1.0 (l 100.0)))
    (is (= 0.75 (l 50.0)))
    (is (= 1500.0 (l 1000 2000 0)))))

(deftest linear-multilinear-comp-test
  (let [l1 (linear [-10.0 0.0 100.0])
        l2 (linear [1000.0 400.0 -1000.0])
        lc (comp (:inverse l2) l1)]
    (is (= 400.0 (lc 0.0)))
    (is (= -1000.0 (lc 100.0)))
    (is (= -300.0 (lc 50.0)))
    (is (= 1000.0 (lc -10.0)))))

(deftest ticks-linear-basics
  (is (= [1.0] (ticks-linear 1 1 1)))
  (is (= [1.0] (ticks-linear 1 1 10)))
  (is (= [] (ticks-linear 0 1 0)))
  (is (= [] (ticks-linear 0 1 -1)))
  (is (= [] (ticks-linear 0 1 ##NaN))))

(deftest ticks-linear-values
  (is (= (ticks-linear  0  1 10) [0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]))
  (is (= (ticks-linear  0  1  9) [0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]))
  (is (= (ticks-linear  0  1  8) [0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]))
  (is (= (ticks-linear  0  1  7) [0.0     0.2     0.4     0.6     0.8     1.0]))
  (is (= (ticks-linear  0  1  6) [0.0     0.2     0.4     0.6     0.8     1.0]))
  (is (= (ticks-linear  0  1  5) [0.0     0.2     0.4     0.6     0.8     1.0]))
  (is (= (ticks-linear  0  1  4) [0.0     0.2     0.4     0.6     0.8     1.0]))
  (is (= (ticks-linear  0  1  3) [0.0                 0.5                 1.0]))
  (is (= (ticks-linear  0  1  2) [0.0                 0.5                 1.0]))
  (is (= (ticks-linear  0  1  1) [0.0                                     1.0]))
  (is (= (ticks-linear  0 10 10) [0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0]))
  (is (= (ticks-linear  0 10  9) [0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0]))
  (is (= (ticks-linear  0 10  8) [0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0]))
  (is (= (ticks-linear  0 10  7) [0.0     2.      4.0     6.0     8.0     10.0]))
  (is (= (ticks-linear  0 10  6) [0.0     2.0     4.0     6.0     8.0     10.0]))
  (is (= (ticks-linear  0 10  5) [0.0     2.0     4.0     6.0     8.0     10.0]))
  (is (= (ticks-linear  0 10  4) [0.0     2.0     4.0     6.0     8.0     10.0]))
  (is (= (ticks-linear  0 10  3) [0.0                 5.0                 10.0]))
  (is (= (ticks-linear  0 10  2) [0.0                 5.0                 10.0]))
  (is (= (ticks-linear  0 10  1) [0.0                                     10.0]))
  (is (= (ticks-linear -10 10 10) [-10.0 -8.0 -6.0 -4.0 -2.0 0.0 2.0 4.0 6.0 8.0 10.0]))
  (is (= (ticks-linear -10 10  9) [-10.0 -8.0 -6.0 -4.0 -2.0 0.0 2.0 4.0 6.0 8.0 10.0]))
  (is (= (ticks-linear -10 10  8) [-10.0 -8.0 -6.0 -4.0 -2.0 0.0 2.0 4.0 6.0 8.0 10.0]))
  (is (= (ticks-linear -10 10  7) [-10.0 -8.0 -6.0 -4.0 -2.0 0.0 2.0 4.0 6.0 8.0 10.0]))
  (is (= (ticks-linear -10 10  6) [-10.0        -5.0       0.0         5.0       10.0]))
  (is (= (ticks-linear -10 10  5) [-10.0        -5.0       0.0         5.0       10.0]))
  (is (= (ticks-linear -10 10  4) [-10.0        -5.0       0.0         5.0       10.0]))
  (is (= (ticks-linear -10 10  3) [-10.0        -5.0       0.0         5.0       10.0]))
  (is (= (ticks-linear -10 10  2) [-10.0                   0.0                   10.0]))
  (is (= (ticks-linear -10 10  1) [                        0.0                       ])))

(deftest ticks-linear-reversed
  (is (= (ticks-linear  0  1 10) (reverse (ticks-linear  1  0 10))))
  (is (= (ticks-linear  0  1  9) (reverse (ticks-linear  1  0  9))))
  (is (= (ticks-linear  0  1  8) (reverse (ticks-linear  1  0  8))))
  (is (= (ticks-linear  0  1  7) (reverse (ticks-linear  1  0  7))))
  (is (= (ticks-linear  0  1  6) (reverse (ticks-linear  1  0  6))))
  (is (= (ticks-linear  0  1  5) (reverse (ticks-linear  1  0  5))))
  (is (= (ticks-linear  0  1  4) (reverse (ticks-linear  1  0  4))))
  (is (= (ticks-linear  0  1  3) (reverse (ticks-linear  1  0  3))))
  (is (= (ticks-linear  0  1  2) (reverse (ticks-linear  1  0  2))))
  (is (= (ticks-linear  0  1  1) (reverse (ticks-linear  1  0  1))))
  (is (= (ticks-linear  0 10 10) (reverse (ticks-linear  10 0 10))))
  (is (= (ticks-linear  0 10  9) (reverse (ticks-linear  10 0  9))))
  (is (= (ticks-linear  0 10  8) (reverse (ticks-linear  10 0  8))))
  (is (= (ticks-linear  0 10  7) (reverse (ticks-linear  10 0  7))))
  (is (= (ticks-linear  0 10  6) (reverse (ticks-linear  10 0  6))))
  (is (= (ticks-linear  0 10  5) (reverse (ticks-linear  10 0  5))))
  (is (= (ticks-linear  0 10  4) (reverse (ticks-linear  10 0  4))))
  (is (= (ticks-linear  0 10  3) (reverse (ticks-linear  10 0  3))))
  (is (= (ticks-linear  0 10  2) (reverse (ticks-linear  10 0  2))))
  (is (= (ticks-linear -10 10 10) (reverse (ticks-linear 10 -10 10))))
  (is (= (ticks-linear  0 10  1) (reverse (ticks-linear  10 0  1))))
  (is (= (ticks-linear -10 10  9) (reverse (ticks-linear 10 -10  9))))
  (is (= (ticks-linear -10 10  8) (reverse (ticks-linear 10 -10  8))))
  (is (= (ticks-linear -10 10  7) (reverse (ticks-linear 10 -10  7))))
  (is (= (ticks-linear -10 10  6) (reverse (ticks-linear 10 -10  6))))
  (is (= (ticks-linear -10 10  5) (reverse (ticks-linear 10 -10  5))))
  (is (= (ticks-linear -10 10  4) (reverse (ticks-linear 10 -10  4))))
  (is (= (ticks-linear -10 10  3) (reverse (ticks-linear 10 -10  3))))
  (is (= (ticks-linear -10 10  2) (reverse (ticks-linear 10 -10  2))))
  (is (= (ticks-linear -10 10  1) (reverse (ticks-linear 10 -10  1)))))

