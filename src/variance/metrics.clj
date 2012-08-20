(ns variance.metrics)

;; Distance metrics

(defstruct point :x :y)

(defn mahalanobis-distance [p q])

(defn manhattan-distance
  "Returns the manhattan/taxicab distance
   between two points x and y"
  [x y]
  (let [[x1 x2] x [y1 y2] y]
    (+ (- x1 x2)) (- y1 y2)))

(defn minkowski-distance []
  ())