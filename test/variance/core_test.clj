(ns variance.core-test
  (:use midje.sweet
        variance.core))

(facts "about mean values"
  (mean [3, 7, 5, 13, 20, 23, 39, 23, 40, 23, 14, 12, 56, 23, 29]) => 22.0
  (mean [3, -7, 5, 13, -2]) => 2.4)

(facts "about median values"
  (median 10 21) => 15
  (median [8, 3, 44, 17, 12, 6]) => 10)

