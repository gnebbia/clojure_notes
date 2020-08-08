#-(and) "

P71 (*) Determine the internal path length of a tree

    We define the internal path length of a multiway tree as the total
    sum of the path lengths from the root to all nodes of the tree. By
    this definition, the tree in the figure of problem P70 has an
    internal path length of 9. Write a predicate ipl(Tree,IPL) for the
    flow pattern (+,-).
   
"

;; A simple direct recursive solution:

(defun multiway-tree-total-path-length (tree so-far)
  "
SO-FAR is the length of path from the root to TREE.
Returns the total length of path from the root to each nodes of TREE.
"
  (reduce (function +)
          (multiway-tree-children tree)
          :key (lambda (node) (multiway-tree-total-path-length node (1+ so-far)))
          :initial-value so-far))

(defun ipl (tree)
  (multiway-tree-total-path-length tree 0))

(assert (= 9 (ipl (parse-multiway-tree-string "AFG^^C^BD^E^^^"))))

;;;; THE END ;;;;
