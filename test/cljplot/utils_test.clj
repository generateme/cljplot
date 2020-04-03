(ns cljplot.utils-test
  (:require [cljplot.utils :refer :all]
            [clojure.test :refer :all]))

(deftest slice-range-test
  (is (= [0.5] (slice-range 1)))
  (is (= [0.0 1.0] (slice-range 2)))
  (is (= [0.0 0.5 1.0] (slice-range 3)))
  (is (= [0.0 (/ 1.0 3.0) (/ 2.0 3.0) 1.0] (slice-range 4)))
  (is (= [0.0 0.25 0.5 0.75 1.0] (slice-range 5)))
  
  (is (= [15.0] (slice-range 1 10 20)))
  (is (= [10.0 20.0] (slice-range 2 10 20)))
  (is (= [10.0 15.0 20.0] (slice-range 3 10 20)))
  (is (= [10.0 12.5 15.0 17.5 20.0] (slice-range 5 10 20))))
