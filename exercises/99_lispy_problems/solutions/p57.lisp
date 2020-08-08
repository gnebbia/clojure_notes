#-(and) "
   
P57 (**) Binary search trees (dictionaries)

    Use the predicate add/3, developed in chapter 4 of the course, to
    write a predicate to construct a binary search tree from a list of
    integer numbers.

    Example:
    * construct([3,2,5,7,1],T).
    T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))
   
    Then use this predicate to test the solution of the problem P56.
    Example:
    * test-symmetric([5,3,18,1,4,12,21]).
    Yes
    * test-symmetric([3,2,5,7,1]).
    No
"



;; Functional solution: we build a new tree, that may refer to subtrees in the old tree.

(defun binary-tree-add-child (tree child leftp)
  "
Returns a new tree like TREE, but where the CHILD in a new leaf tree
either on the left or right of the TREE.  The TREE tree must not have
previously a child in that position.
"
  (assert (not (binary-tree-empty-p tree)))
  (assert (binary-tree-empty-p (if leftp
                                   (binary-tree-left  tree)
                                   (binary-tree-right tree))))
  (make-binary-tree :label (binary-tree-label tree)
                    :left  (if leftp
                               (make-binary-tree :label child)
                               (binary-tree-left  tree))
                    :right (if leftp
                               (binary-tree-right  tree)
                               (make-binary-tree :label child))))


(defun binary-tree-add-item (tree item lessp)
  (if (funcall lessp item (binary-tree-label tree))
      ;; add on the left:
      (if (binary-tree-empty-p (binary-tree-left tree))
          (binary-tree-add-child tree item t)
          (make-binary-tree :label (binary-tree-label tree)
                            :left (binary-tree-add-item (binary-tree-left tree) item lessp)
                            :right (binary-tree-right tree)))
      ;; add on the right:
      (if (binary-tree-empty-p (binary-tree-right tree))
          (binary-tree-add-child tree item nil)
          (make-binary-tree :label (binary-tree-label tree)
                            :left (binary-tree-left tree)
                            :right (binary-tree-add-item (binary-tree-right tree) item lessp)))))

(defun binary-tree-add-items (tree items lessp)
  (if (endp items)
      tree
      (binary-tree-add-items (binary-tree-add-item tree (first items) lessp)
                             (rest items) lessp)))


(defun construct (data lessp)
  (if (endp data)
      (make-empty-binary-tree)
      (binary-tree-add-items (make-binary-tree :label (first data)) (rest data) lessp)))


;; (construct '(3 2 5 7 1) (function <))
;; --> (3 (2 (1 NIL NIL) NIL) (5 NIL (7 NIL NIL)))
;; (binary-tree-symetric-p (construct '(5 3 18 1 4 12 21) (function <)))
;; --> T
;; (binary-tree-symetric-p (construct '(3 2 5 7 1) (function <)))
;; --> T
;; (binary-tree-symetric-p (construct '(1 2 3 4 5) (function <)))
;; --> NIL



;; Procedural solution: the tree is modified in place.

(defun binary-tree-add-child (tree child leftp)
  "
Returns tree.
The tree is modified, with child being set either as a new leaf child, left or right.
either on the left or right of the TREE.  The TREE tree must not have
previously a child in that position.
"
  (assert (not (binary-tree-empty-p tree)))
  (assert (binary-tree-empty-p (if leftp
                                   (binary-tree-left  tree)
                                   (binary-tree-right tree))))

  (if leftp
      (setf (binary-tree-left  tree) (make-binary-tree :label child))
      (setf (binary-tree-right tree) (make-binary-tree :label child)))
  tree)


(defun binary-tree-add-item (tree item lessp)
  "
Returns tree.
Modifies the TREE, adding a new leaf labelled with the ITEM, ordered by LESSP."
  (if (funcall lessp item (binary-tree-label tree))
      ;; add on the left:
      (if (binary-tree-empty-p (binary-tree-left tree))
          (binary-tree-add-child tree item t)
          (binary-tree-add-item (binary-tree-left tree) item lessp))
      ;; add on the right:
      (if (binary-tree-empty-p (binary-tree-right tree))
          (binary-tree-add-child tree item nil)
          (binary-tree-add-item (binary-tree-right tree) item lessp)))
  tree)


(defun binary-tree-add-items (tree items lessp)
  (loop
     :for item :in items
     :do (binary-tree-add-item tree item lessp))
  tree)


(defun construct (data lessp)
  (if (endp data)
      (make-empty-binary-tree)
      (binary-tree-add-items (make-binary-tree :label (first data)) (rest data) lessp)))



(assert (equal (construct '(3 2 5 7 1) (function <))
               '(3 (2 (1 NIL NIL) NIL) (5 NIL (7 NIL NIL)))))
(assert (binary-tree-symetric-p (construct '(5 3 18 1 4 12 21) (function <))))
(assert (binary-tree-symetric-p (construct '(3 2 5 7 1) (function <))))
(assert (binary-tree-symetric-p (construct '(1 2 3 4 5) (function <))))

(assert (equal
         (construct '(n k c a e d g m u p q) (function string<))

         (make-binary-tree
          :label 'n
          :left  (make-binary-tree
                  :label 'k
                  :left  (make-binary-tree
                          :label 'c
                          :left  (make-binary-tree :label 'a)
                          :right (make-binary-tree
                                  :label 'e
                                  :left  (make-binary-tree :label 'd)
                                  :right (make-binary-tree :label 'g)))
                  :right (make-binary-tree :label 'm))
          :right (make-binary-tree
                  :label 'u
                  :left (make-binary-tree
                         :label 'p
                         :right (make-binary-tree :label 'q))))))

;;;; THE END ;;;;
