(ns variance.metrics
  (:use [clojure.set]))

;; Distance metrics

(defstruct point :x :y)

(defn jaccard-index
  "The Jaccard coefficient measures similarity between sample sets,
   and is defined as the size of the intersection divided by the size
   of the union of the sample sets."
   [x y]
   (/ (count (intersection x y)) (count (union x))))

(defn mahalanobis-distance [p q])

(defn manhattan-distance
  "Returns the manhattan/taxicab distance
   between two points x and y"
  [x y]
  (let [[x1 x2] x [y1 y2] y]
    (+ (- x1 x2)) (- y1 y2)))

(defn minkowski-distance []
  ())