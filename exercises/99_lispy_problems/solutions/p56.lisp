#-(and) "
   
P56 (**) Symmetric binary trees

    Let us call a binary tree symmetric if you can draw a vertical
    line through the root node and then the right subtree is the
    mirror image of the left subtree. Write a predicate symmetric/1 to
    check whether a given binary tree is symmetric. Hint: Write a
    predicate mirror/2 first to check whether one tree is the mirror
    image of another. We are only interested in the structure, not in
    the contents of the nodes.

"


;; Solution:

(defun binary-tree-mirror-p (left right)
  (or (and (binary-tree-empty-p left)
           (binary-tree-empty-p right))
      (and (not (binary-tree-empty-p left))
           (not (binary-tree-empty-p right))
           (binary-tree-mirror-p (binary-tree-left  left)
                                 (binary-tree-right right))
           (binary-tree-mirror-p (binary-tree-right left)
                                 (binary-tree-left  right)))))

(defun binary-tree-symetric-p (node)
  (binary-tree-mirror-p (binary-tree-left  node)
                        (binary-tree-right node)))


;; (binary-tree-symetric-p (binary-tree-from-sexp '(x
;;                                                  (x (x nil nil) nil)
;;                                                  (x nil (x nil nil)))))
;; --> T
;; 
;; (binary-tree-symetric-p (binary-tree-from-sexp '(x
;;                                                  (x (x nil nil) nil)
;;                                                  (x (x nil nil) nil))))
;; --> NIL

;;;; THE END ;;;;
