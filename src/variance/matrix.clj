(ns variance.matrix)

;; Matrix math functions

(defrecord Matrix [m])
    
(def A (Matrix. [[1 0] [2 5] [3 1]]))

(def B (Matrix. [[4 0.5] [2 5] [0 1]]))

(defn print-matrix [^Matrix matrix]
  ^{:doc "Print out a matrix in an easy to read format"}
  (doseq [r (:m matrix)]
    (println r)))

(defn rows? [^Matrix m]
  (count (:m m)))

(defn columns? [^Matrix m]
  (count (first (:m m))))

(defn matrix-addition
  [^Matrix m1 ^Matrix m2]
    (map vector (:m m1) (:m m2)))
