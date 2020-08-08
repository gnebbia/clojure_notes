#-(and) "   

P55 (**) Construct completely balanced binary trees

    In a completely balanced binary tree, the following property holds
    for every node: The number of nodes in its left subtree and the
    number of nodes in its right subtree are almost equal, which means
    their difference is not greater than one.
   
    Write a function cbal-tree to construct completely balanced binary
    trees for a given number of nodes. The predicate should generate
    all solutions via backtracking. Put the letter 'x' as information
    into all nodes of the tree.

    Example:
    * cbal-tree(4,T).
    T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
    T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;
    etc......No
"



;; Solution:

(defun binary-tree-count-nodes (node)
  (if (binary-tree-empty-p node)
      0
      (+ 1
         (binary-tree-count-nodes (binary-tree-left  node))
         (binary-tree-count-nodes (binary-tree-right node)))))


(defun binary-tree-balanced-p (node)
  (or (binary-tree-empty-p node)
      (<= (abs (- (binary-tree-count-nodes (binary-tree-left  node))
                  (binary-tree-count-nodes (binary-tree-right node))))
          1)))


;; (binary-tree-balanced-p (binary-tree-from-sexp '(x (x nil nil) (x nil (x nil nil)))))
;; --> T
;; 
;; (binary-tree-balanced-p (binary-tree-from-sexp '(x (x nil nil) (x (x nil nil) nil))))
;; --> T
;; 
;; (binary-tree-balanced-p (binary-tree-from-sexp '(x nil (x (x nil nil) nil))))
;; --> NIL


;;;; THE END ;;;;
