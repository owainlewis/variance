(ns variance.core
  {:doc "A collection of useful math/stat functions"})

(defn mean [values]
  (float
    (/ (reduce + values) (count values))))

(defn median
  "Returns the median value for a sequence or pair of numbers"
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

(def sqrt (fn [x] (Math/sqrt x)))

(defn variance
  "Returns the variance for a collection of values"
  [values]
  (def sqr (fn [x] (* x x)))
  (let [mv (mean values)]
    (/
      (reduce +
        (map #(sqr (- % mv)) values))
          (count values))))
  
(defn standard-deviation [values]
  (sqrt (variance values)))

(defn covariance [data])