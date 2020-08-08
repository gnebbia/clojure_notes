#-(and) "

   
P61A (*) Collect the leaves of a binary tree in a list
    A leaf is a node with no successors. Write a predicate leaves/2 to collect them in a list.
   
    % leaves(T,S) :- S is the list of all leaves of the binary tree T

"

(load "p61")


;; Simple recursive solution:

(defun collect-leaves (tree)
  (cond
    ((binary-tree-empty-p tree)  '())
    ((binary-tree-leaf-p  tree)  (list tree))
    (t (append (collect-leaves (binary-tree-left  tree))
               (collect-leaves (binary-tree-right tree))))))


;; For very deep trees, here is a solution avoiding stack use:

(defun collect-leaves (tree)
  (if (binary-tree-empty-p tree)
      '()
      (loop
         :with stack = (list tree)
         :for node = (pop stack) :then (if (binary-tree-empty-p (binary-tree-left node))
                                           (pop stack)
                                           (binary-tree-left node))
         :while node
         :unless (binary-tree-empty-p (binary-tree-right node))
         :do (push (binary-tree-right node) stack)
         :when (binary-tree-leaf-p node) :collect node)))



;; Doesn't the comparison of p61 and p61a make cry?
;; Here is a parameterized simple recursive solution:

(defun reduce-tree (fun-node fun-leaf tree &key empty-tree-value)
  (cond
    ((binary-tree-empty-p tree)  empty-tree-value)
    ((binary-tree-leaf-p  tree)  (funcall fun-leaf tree))
    (t (funcall fun-node
                tree
                (reduce-tree fun-node fun-leaf (binary-tree-left  tree)
                             :empty-tree-value empty-tree-value)
                (reduce-tree fun-node fun-leaf (binary-tree-right tree)
                             :empty-tree-value empty-tree-value)))))

(defun count-leaves (tree)
  (reduce-tree (lambda (node left right) (declare (ignore node)) (+ left right))
               (lambda (leaf)            (declare (ignore leaf)) 1)
               tree
               :empty-tree-value 0))

(defun collect-leaves (tree)
  (reduce-tree (lambda (node left right) (declare (ignore node)) (append left right))
               (function list)
               tree
               :empty-tree-value '()))


;; And similarly, for very deep trees, here is a parameterized
;; solution avoiding stack use:

(defun reduce-leaves-of-tree (fun-leaf tree &key initial-value)
  (if (binary-tree-empty-p tree)
      initial-value
      (loop
         :with result = initial-value
         :with stack = (list tree)
         :for node = (pop stack) :then (if (binary-tree-empty-p (binary-tree-left node))
                                           (pop stack)
                                           (binary-tree-left node))
         :while node
         :unless (binary-tree-empty-p (binary-tree-right node))
         :do (push (binary-tree-right node) stack)
         :when (binary-tree-leaf-p node)
         :do (setf result (funcall fun-leaf node result))
         :finally (return result))))


(defun count-leaves (tree)
  (reduce-leaves-of-tree (lambda (leaf result) (+ 1 result)) tree :initial-value 0))


(defun collect-leaves (tree)
  (reverse (reduce-leaves-of-tree (function cons) tree :initial-value '())))


;; By the way, notice how the initial recursive solution leads to a
;; more general reduce-tree function.


;;;; THE END ;;;;
