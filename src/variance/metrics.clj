(ns variance.metrics
  (:use [clojure.set]))

;; Distance metrics

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

(defn manhattan-distance
  "Returns the manhattan/taxicab distance
   between two points x and y"
  [x y]
  (let [[x1 x2] x [y1 y2] y]
    (+ (- x1 x2)) (- y1 y2)))

(defn distance-function
  "Returns a memoized distance function. You specify the functions that determine insertion and
  replacement costs. Provided by Spencer Tipping at Factual"
  [replacement-cost-fn insertion-cost-fn]
  (let [memo-table (atom {})]
    (fn distance [as bs]
      (or (get @memo-table [as bs])
          (let [result (cond (empty? as) (reduce + 0.0 (map insertion-cost-fn bs))
                             (empty? bs) (reduce + 0.0 (map insertion-cost-fn as))
                             (= (first as) (first bs)) (distance (rest as) (rest bs))
                             :else (let [[a & a-rest] as
                                         [b & b-rest] bs]
                                     (min (+ (replacement-cost-fn a b) (distance a-rest b-rest))
                                          (+ (insertion-cost-fn b)     (distance as b-rest))
                                          (+ (insertion-cost-fn a)     (distance a-rest bs)))))]
            (swap! memo-table assoc [as bs] result)
            result)))))

(defn hamming-distance
  "Measures the minimum number of substitutions required to
   change one string into the other, or the number of errors
   that transformed one string into the other.
   Hamming distance only counts substitutions, and so is only suitable for equal
   length strings"
  [^String s1 ^String s2]
  (letfn [(zip [x y] (map vector x y))]
    (when (= (count s1) (count s2))
      (let [char-map (zip s1, s2)]
        (reduce + 
          (map (fn [^java.lang.Boolean b]
                 (if (false? b) 1 0))
            (map (fn [[a b]] (= a b)) char-map)))))))

;; TODO

(defn minkowski-distance [])

(defn levenshtein-distance
  "The Levenshtein distance between two strings is defined as the minimum number
   of edits needed to transform one string into the other, with the allowable edit
   operations being insertion, deletion, or substitution of a single character."
  [^String s1 ^String s2]
  (let [m (count s1)
        n (count s2)]))