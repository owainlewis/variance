(ns variance.core
  {:doc "A collection of useful math/stat functions"})

(defn sum
  "Utility function for summing a sequence of values"
  [& values]
  (reduce + 0 values))

;; **************************
;; Averages
;; **************************

(defn arithmetic-mean
  "Return the arithmetic mean for a set of values"
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
        (/ n (reduce +
               (map #(/ %) coll))))))
  ([x y] (harmonic-mean [x y])))

(defn geometric-mean
  "A type of mean or average, which indicates the central tendency or typical
  value of a set of numbers."
  [coll]
  (let [n (count coll)
        sum (reduce * coll)]
    (Math/pow sum (/ n)))) ;; Todo use pow function

(defn median
  "Returns the median value for a sequence or pair of numbers"
  ([coll]
    (let [sorted (sort coll)
        count (count coll)
        mid-point (bit-shift-right count 1)]
      (if (odd? count)
        (nth sorted mid-point)
        (/
          (+ (nth sorted mid-point)
             (nth sorted (dec mid-point))) 2))))
  ([x y]
     (let [r (range x y)]
       (median r))))

(defn mode [coll]
  "A collection of values can have more than one mode in which case it is
   called multimodal or bimodal. Returns the modal value(s)"
  (let [frequency-distribution (frequencies coll)
        sorted (sort-by
                 (comp - second) frequency-distribution)
        mxfreq (second (first sorted))]
    (map first
  (take-while
    (fn [[val freq]]
      (= mxfreq freq)) sorted))))

(defn sqrt [x]   (Math/sqrt x))
(defn sin  [x]   (Math/sin x))
(defn cos  [x]   (Math/cos x))
(defn tan  [x]   (Math/tan x))
(defn log  [x]   (Math/log x))
(defn atan [x]   (Math/atan x))
(defn exp  [x]   (Math/exp x))
(def  E          (Math/E))
(def  PI         (Math/PI))

(defn pow  [base exponent] (Math/pow base exponent))

(def log-with-base
  (fn [base n]
    (/ (Math/log n) (Math/log base))))

(defn log2
  "Log base 2 of n"
  [n] (log-with-base 2 n))

(defn variance
  "Returns the variance for a collection of values.
   A measure of how far a set of numbers is spread out."
  [data]
    (def sqr (fn [x] (* x x)))
    (let [mv (mean data)]
      (/
        (reduce +
          (map
            #(sqr (- % mv)) data))
              (count data))))

(defn standard-deviation
  "In statistics and probability theory, standard deviation (represented by the symbol sigma, σ)
   shows how much variation or dispersion exists from the average (mean, or expected value).
   A low standard deviation indicates that the data points tend to be very close to the mean,
   whereas high standard deviation indicates that the data points are spread out over a large
   range of values."
  [data]
  (sqrt (variance data)))

(defn covariance
  "Returns the covariance of two data sets"
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
  "Returns the range for a collection of numbers"
  [coll]
  (let [sorted (sort coll)
        low (first sorted)
        high (last sorted)]
    (- high low)))

(defn values-less-or-equal
  "Items from values less than or equal to n"
  [values n]
  (take-while
    (fn [x] (<= x n)) values))

(defn percentile-rank
  "Calculates the percentile rank for a val in values"
  [values val]
  (let [a (count (values-less-or-equal values val))
        b (count values)]
    (/ (* 100 a) (double b))))

;; **************************
;; Dot product
;; **************************

(defn dot-product
  "Pairwise product of two vectors that that works for double arrays
   (def ds (double-array (range 3 20)))"
  [^doubles ws ^doubles xs]
  (areduce ws idx sum (float 0)
           (+ sum
             (* (aget ws idx)
                (aget xs idx)))))

(defn dot-p
  "A simple non optimized dot product function"
  [x y]
  (reduce + (map * x y)))

(defn add [& values]
  (reduce + 0 values))

;; **************************
;; Distribution
;; **************************

(defn cumulative-distribution-function
  "Similar to percentile rank but returns a probability in the range 0-1"
  [values val]
  (let [a (count (values-less-or-equal values val))
        b (count values)]
    (double
     (/ a b))))

;; A less verbose name for cumulative-frequency-distribution

(def cdf cumulative-distribution-function)

(defn cdf-range
  "Maps cdf over a range of numbers"
  [values start end]
  (let [func (partial cdf values)]
    (map
      (fn [val] (func val))
      (range start end))))

(defn sigmoid
  "A logistic function or logistic sigmoid curve"
  [t]
  (/ 1 (+ 1 (exp t))))

(defn quadratic-formula
  "Use quadratic formula to solve quadratic equasion for x
   e.g Solve 5x² + 6x + 1 = 0 -> x = [ -b ± √(b2-4ac) ] / 2a"
  [a b c]
  (let [divisor (* 2 a)
        x (- 0 b)
        y (Math/sqrt (- (* b b) (* 4 a c)))]
    (vector
     (/ (- x y) divisor)
     (/ (+ x y) divisor))))

(defn mapstats
  "Inspired by a public gist by Jason Wolfe
  Takes a map {:xs xs} and returns a map
  of simple univariate statistics of xs
  (mapstats {:xs [1 2 3]})"
  [{:keys [xs]}]
  (let [n  (count xs)
        m  (/ (reduce + xs) n)
        m2 (/ (reduce + (map #(* % %) xs)) n)
        v  (- m2 (* m m))]
    {:n n   ; count
     :m m   ; mean
     :m2 m2 ; mean square
     :v v   ; variance
    }))

;; *************************
;; Chi Square
;; **************************

(defn chi-square
  "Assumes data to be in the form
  [[x1 observed, x1 expected] [x2 observed, x2 expected]].
   The Chi-square test computes the sum of the squares of the differences in values"
  [values]
  (reduce + 0
    (map
      (fn [[observed expected]]
        (double
          (/ (pow (- observed expected) 2)
             expected)))
    values)))

;; **************************
;; Entropy
;; **************************

(defn shannon-entropy
  "Calculates the Shannon entropy of a data set"
  [data]
  (let [freq (frequencies data)
        m    (vals freq)
        n    (reduce + m)]
  (- (reduce +
    (map (fn [v]
           (let [a (/ v n)]
             (* a (log2 a)))) m)))))