(ns variance.metrics
  (:use [clojure.set]))

;; Distance metrics

(defstruct 
  point :x :y)

(defn jaccard-index
  "The Jaccard coefficient measures similarity between sample sets,
   and is defined as the size of the intersection divided by the size
   of the union of the sample sets."
   [x y]
   (/ (count (intersection x y)) (count (union x))))

(defn jaccard-distance
  "measures dissimilarity between sample sets, is complementary to the Jaccard
   coefficient and is obtained by subtracting the Jaccard coefficient from 1"
  [a b] (- 1 (jaccard-index a b)))

(defn manhattan-distance0
  "Returns the manhattan/taxicab distance
   between two points x and y"
  [x y]
  (let [[x1 x2] x [y1 y2] y]
    (+ (- x1 x2)) (- y1 y2)))

(defn minkowski-distance [])

(defn levenshtein-distance
  "The Levenshtein distance between two strings is defined as the minimum number
   of edits needed to transform one string into the other, with the allowable edit
   operations being insertion, deletion, or substitution of a single character. "
  [^String s1 ^String s2]
  (let [m (count s1)
        n (count s2)]))