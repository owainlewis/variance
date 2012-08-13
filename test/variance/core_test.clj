(ns variance.core-test
  (:use midje.sweet
        variance.core))

(facts "about mean values"
  (mean [3, 7, 5, 13, 20, 23, 39, 23, 40, 23, 14, 12, 56, 23, 29]) => 22.0)