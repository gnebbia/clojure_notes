#-(and) "

P61 (*) Count the leaves of a binary tree
    A leaf is a node with no successors. Write a predicate count-leaves/2 to count them.
   
    % count-leaves(T,N) :- the binary tree T has N leaves

"

(load "p54a")
(load "p55")
(load "p56")
(load "p57")


(defun binary-tree-leaf-p (node)
  (and (binary-tree-p node)
       (binary-tree-empty-p (binary-tree-left  node))
       (binary-tree-empty-p (binary-tree-right node))))


;; Simple recursive solution:

(defun count-leaves (tree)
  (cond
    ((binary-tree-empty-p tree)  0)
    ((binary-tree-leaf-p  tree)  1)
    (t (+ (count-leaves (binary-tree-left  tree))
          (count-leaves (binary-tree-right tree))))))


;; For very deep trees, here is a solution avoiding stack use:

(defun count-leaves (tree)
  (if (binary-tree-empty-p tree)
      0
      (loop
         :with stack = (list tree)
         :for node = (pop stack) :then (if (binary-tree-empty-p (binary-tree-left node))
                                           (pop stack)
                                           (binary-tree-left node))
         :while node
         :unless (binary-tree-empty-p (binary-tree-right node))
         :do (push (binary-tree-right node) stack)
         :when (binary-tree-leaf-p node) :count 1)))


;;;; THE END ;;;;
