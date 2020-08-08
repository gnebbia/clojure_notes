#-(and) "

Binary Trees

A binary tree is either empty or it is composed of a root element and
two successors, which are binary trees themselves.  [p67]   

In Lisp we represent the empty tree by 'nil' and the non-empty tree by
the list (X L R), where X denotes the root node and L and R denote the
left and right subtree, respectively. The example tree depicted
opposite is therefore represented by the following list:

(a (b (d nil nil) (e nil nil)) (c nil (f (g nil nil) nil)))

Other examples are a binary tree that consists of a root node only:

(a nil nil) or an empty binary tree: nil.

You can check your predicates using these example trees. They are given as test cases in p54.lisp.

P54A (*) Check whether a given term represents a binary tree
    Write a predicate istree which returns true if and only if its argument is a list representing a binary tree.
    Example:
    * (istree (a (b nil nil) nil))
    T
    * (istree (a (b nil nil)))
    NIL
"

;; Notice that  there are a lot of ways to represent trees.
;; Therefore it important to use a functional abstraction, to avoid
;; writing code dependant on a specific representation.
;;
;; Also, in the following problems, references to prolog terms are not
;; translated to lisp, and terms for trees are given as:
;; 
;;     T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) 
;; 
;; We could translate this as having the symbol BINARY-TREE prefixing the
;; lists representing tree nodes:
;; 
;;     (BINARY-TREE a (BINARY-TREE b nil nil) (BINARY-TREE c nil (BINARY-TREE f nil)))
;; 
;; This can be trivially implemented by adding the :NAMED option to defstruct, in which
;; case an implementation of BINARY-TREE-P is provided by destruct.


(defstruct (binary-tree (:type list) :named)
  label left right)
;; This defstruct defines the following functional abstraction:
;; (make-binary-tree :label label :left left :right right)
;; (binary-tree-label node)
;; (binary-tree-left  node)
;; (binary-tree-right node)
;; (copy-binary-tree node) ; shallow copy.
;; We add:
(defun make-empty-binary-tree () 'nil)
(defun binary-tree-empty-p (node) (null node))
(defun binary-tree-p    (node)
  (or (binary-tree-empty-p node)
      (let ((nodes '()))
        (labels ((check-node (node)
                   (and (listp node)
                        (not (member node nodes)) ; check circular structures.
                        (progn (push node nodes) t)
                        (eql 3 (list-length node))
                        (or (null (second node)) (check-node (second node)))
                        (or (null (third  node)) (check-node (third  node))))))
          (check-node node)))))
;; We add:
(defun binary-tree-from-sexp (sexp) sexp)
;; it would be good form to use it, so that if we change the
;; implementation of the above functional abstraction, we can still
;; use the tree sexps defined by for these problems:
;; (binary-tree-p (binary-tree-from-sexp '(a (b (d nil nil) (e nil nil)) (c nil nil))))


(defun binary-tree-height (tree)
  (if (binary-tree-empty-p tree)
      0
      (+ 1 (max (binary-tree-height (binary-tree-left tree))
                (binary-tree-height (binary-tree-right tree))))))




(defparameter *tree-0* 'nil)
(defparameter *tree-1* '(a nil nil))
(defparameter *tree-2* '(a (b (d nil nil) (e nil nil)) (c nil (f (g nil nil) nil))))


;; Solution:

(defun istree (object)
  (binary-tree-p object))


(assert (equal (mapcar (function istree)
                       (list *tree-0* *tree-1* *tree-2*
                             '(a b c)
                             '(a (nil 1 2) (nil 3 4) d)
                             '(a (nil nil nil) (nil nil nil) d)))
               '(T T T NIL NIL NIL)))

;;;; THE END ;;;;
