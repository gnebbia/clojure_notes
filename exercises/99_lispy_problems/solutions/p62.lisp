#-(and) "
   
P62 (*) Collect the internal nodes of a binary tree in a list

    An internal node of a binary tree has either one or two non-empty
    successors. Write a predicate internals/2 to collect them in a
    list.
   
    % internals(T,S) :- S is the list of internal nodes of the binary tree T.
"

(load "p61a")

;; Simple (trivial!) solution using reduce-tree defined in p61a:

(defun collect-internal-nodes (tree)
  (reduce-tree (lambda (node left right) (cons node (append left right)))
               (constantly '()) tree :empty-tree-value '()))

;;;; THE END ;;;;

