(ns variance.trees)

;; **************************
;; Binary trees
;; **************************

(defrecord TreeNode
  [root left right])

(defn tree
  "Create a tree with a root node set"
  [root]
  (TreeNode. root nil nil))

(defn tree-append [t val]
  (cond
    ;; Empty tree 
    (nil? t)          (TreeNode. val nil nil)
    ;; Go left 
    (< val (:root t)) (TreeNode. (:root t) (tree-append (:left t) val) (:right t))
    ;; Go right
    :else             (TreeNode. (:root t) (:left t) (tree-append (:right t) val))))

(defn in-order-traversal [t]
  (when t
    (concat
      (in-order-traversal (:left t)) [(:root t)]
      (in-order-traversal (:right t)))))

(defn height
  "Returns the height of a binary tree"
  [t]
  (if (nil? (:root t))
    0
    (+ 1
      (max
        (height (:left t))
        (height (:right t))))))

(defn tree-build
  "Helper function for building binary trees
   from a sequence of values"
  [[car & cdr]]
  (let [t (tree car)]
    (reduce tree-append t cdr)))

(comment (tree-build [2 4 1 6 7]))
