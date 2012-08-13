(ns variance.core
  {:doc "A collection of useful math/stat functions"})

(defn mean
  {:doc "Return the mean for a set of values"}
  [values]
  (float
    (/ (reduce + values) (count values))))

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

(defn sqrt
  "Returns the square root of x"
  [x]
  (Math/sqrt x))

(defn sin [x]
  {:doc ""}
  (Math/sin x))

(defn cos
  {:doc "Returns the cosine of x"}
  [x]
  (Math/cos x))

(defn tan
  {:doc "Returns the tangent of x"}
  [x]
  (Math/tan x))

(defn atan
  {:doc "Returns the arc tangent of x"}
  [x]
  (Math/atan x))

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
              
(defn variance
  {:doc "Returns the variance for a collection of values.
   A measure of how far a set of numbers is spread out.
   Could probably avoid doing a numeric value check here to make
   things faster"}
  [values]
  (when (every? number? values)
    (def sqr (fn [x] (* x x)))
    (let [mv (mean values)]
      (/
        (reduce +
          (map #(sqr (- % mv)) values))
            (count values)))))
  
(defn standard-deviation [values]
  {:doc "In statistics and probability theory, standard deviation (represented by the symbol sigma, σ)
   shows how much variation or dispersion exists from the average (mean, or expected value).
   A low standard deviation indicates that the data points tend to be very close to the mean,
   whereas high standard deviation indicates that the data points are spread out over a large
   range of values."}
  (sqrt (variance values)))

(defn covariance [data])

(defn gini-coefficient [])