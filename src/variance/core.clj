(ns variance.core
  {:doc "A collection of useful math/stat functions"}
  (:use [variance.metrics]))
  
(defn dot-product
  "Pairwise product of two vectors that that works for double arrays
   (def ds (double-array (range 3 20)))"
  [^doubles ws ^doubles xs]
  (areduce ws idx sum (float 0)
           (+ sum (* (aget ws idx)
                     (aget xs idx)))))

(defn dot-p
  "A simple non optimized dot product function"
  [x y]
  (reduce + (map * x y)))

(defn arithmetic-mean
  {:doc "Return the arithmetic mean for a set of values"}
  ([coll]
    (double
      (/ (reduce + coll) (count coll))))
  ([x y]
    (arithmetic-mean [x y])))

(def mean arithmetic-mean)

(defn harmonic-mean
  "The harmonic mean is the reciprocal of the arithmetic mean of the reciprocals.
   As it tends strongly toward the least elements of the list,
   it may (compared to the arithmetic mean) mitigate the influence of large outliers
   and to increase the influence of small values."
  ([coll]
     (let [n (count coll)]
       (double 
        (/ n (reduce + (map #(/ %) coll))))))
  ([x y] (harmonic-mean [x y])))
     
(defn geometric-mean
  "A type of mean or average, which indicates the central tendency or typical
  value of a set of numbers."
  [coll]
  (let [n (count coll)
        sum (reduce * coll)]
    (Math/pow sum (/ n))))

(defn median
  {:doc "Returns the median value for a sequence or pair of numbers"}
  ([coll]
    (let [sorted (sort coll)
        count (count coll)
        mid-point (bit-shift-right count 1)]
      (if (odd? count)
        (nth sorted mid-point)
        (/ (+ (nth sorted mid-point) (nth sorted (dec mid-point))) 2))))
  ([x y]
     (let [r (range x y)]
       (median r))))

(defn mode [coll]
  "A collection of values can have more than one mode in which case it is
   called multimodal or bimodal. Returns the modal value(s) for a
   collection of values"
  (let [frequency-distribution (frequencies coll)
        sorted (sort-by (comp - second) frequency-distribution)
        mxfreq (second (first sorted))]
    (map first
  (take-while (fn [[val freq]]
    (= mxfreq freq)) sorted))))

(defn sqrt [x]   (Math/sqrt x))
(defn sin  [x]   (Math/sin x))
(defn cos  [x]   (Math/cos x))
(defn tan  [x]   (Math/tan x))
(defn atan [x]   (Math/atan x))
(defn pow  [x y] (Math/pow x y))
(defn exp  [x]   (Math/exp x))
(def  E          (Math/E))
(def  PI         (Math/PI))

(defn variance
  {:doc "Returns the variance for a collection of values.
   A measure of how far a set of numbers is spread out."}
  [data]
    (def sqr (fn [x] (* x x)))
    (let [mv (mean data)]
      (/
        (reduce +
          (map #(sqr (- % mv)) data))
            (count data))))
  
(defn standard-deviation [data]
  {:doc "In statistics and probability theory, standard deviation (represented by the symbol sigma, σ)
   shows how much variation or dispersion exists from the average (mean, or expected value).
   A low standard deviation indicates that the data points tend to be very close to the mean,
   whereas high standard deviation indicates that the data points are spread out over a large
   range of values."}
  (sqrt (variance data)))

(defn covariance
  {:doc "Returns the covariance of two data sets"}
  [data1 data2]
  (let [n (count data1)
        mean1 (mean data1)
        mean2 (mean data2)]
    (reduce + 
      (map (fn [[x y]]
             (let [a (- x mean1)
                   b (-  y mean2)]
             (/ (* a b) n)))
        (zipmap data1 data2)))))

(defn rng
  ^{:doc "Returns the range for a collection of numbers"}
  [coll]
  (let [sorted (sort coll)
        low (first sorted)
        high (last sorted)]
    (- high low)))

;; TODO

(defn gini-coefficient [])