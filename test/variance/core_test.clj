(ns variance.core-test
  (:use midje.sweet
        clojure.test
        variance.core))

(deftest dot-product-test
(facts "about dot product"
  (dot-p [1 3 -5] [4 -2 -1]) => 3
  (dot-product (double-array [1 3 -5])
             (double-array [4 -2 -1])) => 3.0))

(deftest mean-value-test
(facts "about mean values"
  (mean [3, 7, 5, 13, 20, 23, 39, 23, 40, 23, 14, 12, 56, 23, 29])
     => 22.0
  (mean [3, -7, 5, 13, -2])
     => 2.4))

(deftest median-test
(facts "about median values"
  (median 10 21) => 15
  (median [8, 3, 44, 17, 12, 6]) => 10))

(deftest mode-test
  (facts "about modal values"
         (mode [1 2 3 3 3 4 5]) => '(3)
         (mode [1 1 2 3]) => '(1)
         (mode [1 1 2 2 3 3]) => '(1 2 3)
         (mode [1 2.1 2.1 2.3 2.3 3.5]) => '(2.1 2.3)))

(deftest range-test
(facts "about rng"
  (rng [3 6 6 6 7 9 11 11 13]) => 10))

(deftest variance-test
(facts "about variance"
  (int (variance [600 470 170 430 300])) => 21704))

(deftest standard-deviation-test
(facts "about standard-deviation"
  (int (standard-deviation [600 470 170 430 300]))
    => 147))