(ns variance.core
  {:doc "A collection of useful math/stat functions"})

(defn mean
  {:doc "Return the mean for a set of values"}
  [values]
  (double
    (/ (reduce + values) (count values))))

(defn harmonic-mean
  "The harmonic mean is the reciprocal of the arithmetic mean of the reciprocals.
   As it tends strongly toward the least elements of the list,
   it may (compared to the arithmetic mean) mitigate the influence of large outliers
   and to increase the influence of small values."
  ([coll]
     (let [n (count coll)]
       (double 
         (/ n (reduce + (map #(/ 1 %) coll)))))))

(defn median
  {:doc "Returns the median value for a sequence or pair of numbers"}
  ([data]
    (let [sorted (sort data)
        count (count data)
        mid-point (bit-shift-right count 1)]
      (if (odd? count)
        (nth sorted mid-point)
        (/ (+ (nth sorted mid-point) (nth sorted (dec mid-point))) 2))))
  ([x y]
     (let [r (range x y)]
       (median r))))

(defn sqrt [x] (Math/sqrt x))
(defn sin  [x] (Math/sin x))
(defn cos  [x] (Math/cos x))
(defn tan  [x] (Math/tan x))
(defn atan [x] (Math/atan x))
  
(defn variance
  {:doc "Returns the variance for a collection of values.
   A measure of how far a set of numbers is spread out.
   Could probably avoid doing a numeric value check here to make
   things faster"}
  [data]
  (when (every? number? data)
    (def sqr (fn [x] (* x x)))
    (let [mv (mean data)]
      (/
        (reduce +
          (map #(sqr (- % mv)) data))
            (count data)))))
  
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

(defn gini-coefficient [])

(defn rng
  ^{:doc "Returns the range for a collection of numbers"}
  [coll]
  (let [sorted (sort coll)
        low (first sorted)
        high (last sorted)]
    (- high low)))

(defn interquartile-range
  ^{:doc "" }
  [data]
  (let [sorted (sort data)
        q1 0
        q2 0
        q3 0]))
              