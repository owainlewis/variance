(ns variance.metrics
  (refer-clojure :exclude [])
  (:use [clojure.set]))

(defn ngrams [n ^String s]
  (into []
    (map (fn [x] (apply str x))
      (partition n 1 s))))

;; Todo redefine this in terms of the dice coefficient abstraction
(defn dice-coefficient-str [x y]
   {:pre [(and (string? x) (string? y))]}
   (let [unique-bigrams (map #(into #{} (ngrams 2 %)) (vector x y))
         [nx ny] (map count unique-bigrams)]
    (double
      (/ 
        (variance.core/dot-p [2]
            [(count (apply clojure.set/intersection unique-bigrams))])
        (reduce + [nx ny])))))
                 
(defn jaccard-index
  "The Jaccard coefficient measures similarity between sample sets,
   and is defined as the size of the intersection divided by the size
   of the union of the sample sets."
   [p q]
   (/ (count (intersection p q))
        (count (union p))))

(defn jaccard-distance
  "measures dissimilarity between sample sets, is complementary to the Jaccard
   coefficient and is obtained by subtracting the Jaccard coefficient from 1"
  [p q] (- 1 (jaccard-index p q)))

(defn euclidean-distance
  "returns the euclidean distance between two points.
   Expects a and b to be a set of coordinates [x y]"
  [p q]
  (letfn [(square [x] (* x x))]
    (/ 1 (+ 1 (Math/sqrt
      (+ (square (- (first p) (first q)))
        (square (- (second p) (second q)))))))))

;; TODO check this!

(defn manhattan-distance
  "Returns the manhattan/taxicab distance
   between two points x and y"
  [p q]
  (let [[x1 x2] p [y1 y2] q]
    (+ (- x1 x2))
      (- y1 y2)))

(defn distance-graph2
  "Return a map of various distance functions
   e.g (distance-graph [1.4 0.5] [0.3 6.3])"
  [p q]
  (let [euclidean (euclidean-distance p q)
        manhattan (manhattan-distance p q)]
    {:euclidean euclidean
     :manhattan manhattan}))

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
            (map
             (fn [[a b]] (= a b)) char-map)))))))
