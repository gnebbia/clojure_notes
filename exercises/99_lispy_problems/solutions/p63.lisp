#-(and) "
   
P63 (**) Construct a complete binary tree

    A complete binary tree with height H is defined as follows: The
    levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e
    2**(i-1) at the level i, note that we start counting the levels
    from 1 at the root). In level H, which may contain less than the
    maximum possible number of nodes, all the nodes are
    \"left-adjusted\". This means that in a levelorder tree traversal
    all internal nodes come first, the leaves come second, and empty
    successors (the nil's which are not really nodes!) come last.
   
    Particularly, complete binary trees are used as data structures
    (or addressing schemes) for heaps.
   
    We can assign an address number to each node in a complete binary
    tree by enumerating the nodes in levelorder, starting at the root
    with number 1. In doing so, we realize that for every node X with
    address A the following property holds: The address of X's left
    and right successors are 2*A and 2*A+1, respectively, supposed the
    successors do exist. This fact can be used to elegantly construct
    a complete binary tree structure. Write a predicate
    complete-binary-tree/2 with the following specification:
   
    % complete-binary-tree(N,T) :- T is a complete binary tree with N nodes. (+,?)
   
    Test your predicate in an appropriate way.

"
(load "p54a")

(defun complete-binary-tree-upto (a n)
  (make-binary-tree :label a
                    :left  (if (<= (* 2 a) n)
                               (complete-binary-tree-upto (* 2 a) n)
                               (make-empty-binary-tree))
                    :right (if (<= (1+ (* 2 a)) n)
                               (complete-binary-tree-upto (1+ (* 2 a)) n)
                               (make-empty-binary-tree))))


(defun complete-binary-tree (n)
  (complete-binary-tree-upto 1 n))


;; (loop :for n :to 7 :do (princ (draw-tree (complete-binary-tree n))))


;;;; THE END ;;;;
