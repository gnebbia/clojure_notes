#-(and) "

Multiway Trees

A multiway tree is composed of a root element and a (possibly empty)
set of successors which are multiway trees themselves. A multiway tree
is never empty. The set of successor trees is sometimes called a
forest.

[p70]   

In Prolog we represent a multiway tree by a term t(X,F), where X
denotes the root node and F denotes the forest of successor trees (a
Prolog list). The example tree depicted opposite is therefore
represented by the following Prolog term:

T = t(a,[t(f,[t(g,[])]),t(c,[]),t(b,[t(d,[]),t(e,[])])])


"

;; In lisp we could represent a multiway tree in multiple ways.
;; Let's just abstract it away using defstruct.


(defstruct (multiway-tree
             (:predicate non-empty-multiway-tree-p))
  
  label
  children)

;; Again, if lists are wanted instead of structures, (:type list) can
;; be used; if vectors, then (:type vector).  In both cases, if the
;; list or vector must start with the symbol MULTIWAY-TREE, the :named
;; option can be added.


(defun make-empty-multiway-tree ()
  'nil)
(defun empty-multiway-tree-p (tree)
  (null tree))

(defun multiway-tree-p (tree)
  (or (empty-multiway-tree-p tree)
      (non-empty-multiway-tree-p tree)))



#-(and) "

P70B (*) Check whether a given term represents a multiway tree

    Write a predicate istree/1 which succeeds if and only if its
    argument is a Prolog term representing a multiway tree.

    Example:
    * istree(t(a,[t(f,[t(g,[])]),t(c,[]),t(b,[t(d,[]),t(e,[])])])).
    Yes
"

;; Badass solution:

(defun istree (tree)
  (multiway-tree-p tree))


;; In practice, nothing more than the badass solution is needed.  For
;; the exercise, we may check that the children are multiway trees
;; too.

(defun istree (tree)
  (cond
    ((empty-multiway-tree-p tree) t)
    ((non-empty-multiway-tree-p tree)
     (every (function istree) (multiway-tree-children tree)))))


;; Actually, in presence of circular structures, the above istree may
;; not terminate.  Since those exercices are boring, we'll implement
;; an istree that checks for circular structures too:


(defun istree (tree)
  (let ((nodes (make-hash-table)))
    (labels ((multiway-node-p (node)
               (cond
                 ((empty-multiway-tree-p node)           t)
                 ((not (non-empty-multiway-tree-p node))
                  (return-from istree (values nil :non-tree node))) ; short circuit exit
                 ((gethash node nodes)
                  (return-from istree (values nil :circular node))) ; short circuit exit
                 (t
                  (setf (gethash node nodes) t)
                  (every (function multiway-node-p) (multiway-tree-children node))))))
      (multiway-node-p tree))))


(let* ((child (make-multiway-tree :label 'child))
       (root  (make-multiway-tree :label 'root :children (list child))))
  (setf (multiway-tree-children child) (list root))
  (assert (equal (list nil :circular root) (multiple-value-list (istree root)))))

(let* ((child (make-multiway-tree :label 'child :children '(a b c)))
       (root  (make-multiway-tree :label 'root :children (list child))))
  (assert (equal '(nil :non-tree a) (multiple-value-list (istree root)))))

(let* ((child (make-multiway-tree
               :label 'child
               :children (list (make-multiway-tree :label 'a)
                               (make-multiway-tree :label 'b)
                               (make-multiway-tree :label 'c))))
       (root  (make-multiway-tree :label 'root :children (list child))))
  (assert (istree root)))


;; Notice that CL provides for each structure a printer function
;; producing a readable form of the structure:
;;
;; (let* ((child (make-multiway-tree
;;                :label 'child
;;                :children (list (make-multiway-tree :label 'a)
;;                                (make-multiway-tree :label 'b)
;;                                (make-multiway-tree :label 'c))))
;;        (root  (make-multiway-tree :label 'root :children (list child))))
;;   root)
;; --> #S(MULTIWAY-TREE
;;        :LABEL ROOT
;;        :CHILDREN (#S(MULTIWAY-TREE
;;                      :LABEL CHILD
;;                      :CHILDREN (#S(MULTIWAY-TREE :LABEL A :CHILDREN NIL)
;;                                   #S(MULTIWAY-TREE :LABEL B :CHILDREN NIL)
;;                                   #S(MULTIWAY-TREE :LABEL C :CHILDREN NIL)))))
;; 
;; 
;; 
;; So we can also write literal multiway-trees as:
;; 
;; #S(multiway-tree :label example :children (#S(multiway-tree :label a) #S(multiway-tree :label b))) 
;; --> #S(MULTIWAY-TREE :LABEL EXAMPLE
;;                      :CHILDREN (#S(MULTIWAY-TREE :LABEL A :CHILDREN NIL)
;;                                   #S(MULTIWAY-TREE :LABEL B :CHILDREN NIL)))


;;;; END ;;;;

