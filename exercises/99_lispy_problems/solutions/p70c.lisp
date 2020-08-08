#-(and) "
   
P70C (*) Count the nodes of a multiway tree

    Write a predicate nnodes/1 which counts the nodes of a given multiway tree.
    Example:
    * nnodes(t(a,[t(f,[])]),N).
    N = 2
   
    Write another version of the predicate that allows for a flow pattern (o,i).

"
(load "p70b")


(defun multiway-tree-count-nodes (tree)
  (cond
    ((empty-multiway-tree-p tree)
     0)
    ((non-empty-multiway-tree-p tree)
     (+ 1 (reduce (function multiway-tree-count-nodes)
                  (multiway-tree-children tree))))
    (t
     (error "Not a multiway tree: ~S" tree))))




;; The other version of the prolog predicate generates all the trees
;; that have the given number of nodes.


(defun change (n)
  (cons (list n)
        (loop
           :for i :from 1 :below n
           :for subchanges = (change i)
           :nconc (mapcar (lambda (subchange)
                            (cons (- n i) subchange))
                          subchanges))))

(defun cross-product (sets)
  "
SETS is a list of lists.
Returns a list containing each one element taken from each lists in SETS.
"
  (cond
    ((endp sets)         '())
    ((endp (rest sets))  (mapcar (function list) (first sets)))
    (t (mapcan (lambda (crosses)
                 (mapcan (lambda (item)
                           (list (cons item crosses)))
                         (first sets)))
               (cross-product (rest sets))))))

;; (cross-product '())
;; (cross-product '((a1 a2) (b1 b2)))
;; (cross-product '((a1 a2) (b1 b2 b3) (c1 c2)))


;; Notice that we consider that the order of the children matters,
;; but the identity of the children does not.
;;
;; So a node with two children, the first of 2 nodes, and the other of
;; 1 node, will be different from a node with two children, the first
;; of 1 node and the other of 2 nodes.

(defun generate-multiway-trees-with-nodes (node-count next-label)
  "Return a list of multiway-trees with NODE-COUNT nodes."
  (case node-count
    ((0) (list (make-empty-multiway-tree)))
    ((1) (list (make-multiway-tree :label (funcall next-label))))
    (otherwise
     (loop
        :with subtrees = (coerce
                          (loop
                             :for remaining-count :below node-count
                             :collect (generate-multiway-trees-with-nodes remaining-count next-label))
                          'vector)
        :for change :in (change (1- node-count))
        :nconc (mapcar (lambda (children)
                         (make-multiway-tree
                          :label (funcall next-label)
                          :children children))
                       (cross-product (mapcar (lambda (children-count) (aref subtrees children-count))
                                              change)))))))


;; (generate-multiway-trees-with-nodes 4 (let ((n 0)) (lambda () (incf n))))
;; --> 
;; (#S(MULTIWAY-TREE
;;     :LABEL 9
;;     :CHILDREN (#S(MULTIWAY-TREE
;;                   :LABEL 7
;;                   :CHILDREN (#S(MULTIWAY-TREE
;;                                 :LABEL 6
;;                                 :CHILDREN (#S(MULTIWAY-TREE
;;                                               :LABEL 5
;;                                               :CHILDREN NIL)))))))
;;    #S(MULTIWAY-TREE
;;       :LABEL 10
;;       :CHILDREN (#S(MULTIWAY-TREE
;;                     :LABEL 8
;;                     :CHILDREN (#S(MULTIWAY-TREE
;;                                   :LABEL 4
;;                                   :CHILDREN NIL)
;;                                  #S(MULTIWAY-TREE
;;                                     :LABEL 4
;;                                     :CHILDREN NIL)))))
;;    #S(MULTIWAY-TREE
;;       :LABEL 11
;;       :CHILDREN (#S(MULTIWAY-TREE
;;                     :LABEL 3
;;                     :CHILDREN (#S(MULTIWAY-TREE
;;                                   :LABEL 2
;;                                   :CHILDREN NIL)))
;;                    #S(MULTIWAY-TREE
;;                       :LABEL 1
;;                       :CHILDREN NIL)))
;;    #S(MULTIWAY-TREE
;;       :LABEL 12
;;       :CHILDREN (#S(MULTIWAY-TREE
;;                     :LABEL 1
;;                     :CHILDREN NIL)
;;                    #S(MULTIWAY-TREE
;;                       :LABEL 3
;;                       :CHILDREN (#S(MULTIWAY-TREE
;;                                     :LABEL 2
;;                                     :CHILDREN NIL)))))
;;    #S(MULTIWAY-TREE
;;       :LABEL 13
;;       :CHILDREN (#S(MULTIWAY-TREE
;;                     :LABEL 1
;;                     :CHILDREN NIL)
;;                    #S(MULTIWAY-TREE
;;                       :LABEL 1
;;                       :CHILDREN NIL)
;;                    #S(MULTIWAY-TREE
;;                       :LABEL 1
;;                       :CHILDREN NIL))))

;;;; THE END ;;;;
