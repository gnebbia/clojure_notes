#-(and) "
P67 (**) A string representation of binary trees
    [p67]   
   
    Somebody represents binary trees as strings of the following type
    (see example opposite):
   
    a(b(d,e),c(,f(g,)))
   
    a) Write a Prolog predicate which generates this string
    representation, if the tree is given as usual (as nil or t(X,L,R)
    term). Then write a predicate which does this inverse; i.e. given
    the string representation, construct the tree in the usual
    form. Finally, combine the two predicates in a single predicate
    tree-string/2 which can be used in both directions.
   
    b) Write the same predicate tree-string/2 using difference lists
    and a single predicate tree-dlist/2 which does the conversion
    between a tree and a difference list in both directions.
   
    For simplicity, suppose the information in the nodes is a single
    letter and there are no spaces in the string.
   
"

(load "rdp")
(use-package "COM.INFORMATIMAGO.RDP")


;; This is not funny...


;; Badass solution.  In lisp, we can just use print and read to
;; serialize and deserialize printable readably lisp objects:

(defun binary-tree-to-string (tree)
  (prin1-to-string tree))

(defun binary-tree-from-string (string)
  (read-from-string string))

;; (binary-tree-to-string (construct '(n k c a h g e m u p s q) (function string<)))
;; --> "(N (K (C (A NIL NIL) (H (G (E NIL NIL) NIL) NIL)) (M NIL NIL)) (U (P NIL (S (Q NIL NIL) NIL)) NIL))"
;; 
;; (binary-tree-from-string "(N (K (C (A NIL NIL) (H (G (E NIL NIL) NIL) NIL)) (M NIL NIL)) (U (P NIL (S (Q NIL NIL) NIL)) NIL))")
;; --> (N (K (C (A NIL NIL) (H (G (E NIL NIL) NIL) NIL)) (M NIL NIL)) (U (P NIL (S (Q NIL NIL) NIL)) NIL))
;;     99




;; Solution p67 a).  Generating a string is trivial:

(defun binary-tree-to-string (tree)
  (cond
    ((binary-tree-empty-p tree)
     "")
    ;; Since the wanted syntax is irregular,
    ;; [it wants  a(b,c) instead of a(b(,),c(,))],
    ;; we need to add this special case:
    ((and (binary-tree-empty-p (binary-tree-left  tree))
          (binary-tree-empty-p (binary-tree-right tree)))
     (prin1-to-string (binary-tree-label tree)))
    (t
     (format nil "~A(~A,~A)"
             (binary-tree-label tree)
             (binary-tree-to-string (binary-tree-left  tree))
             (binary-tree-to-string (binary-tree-right tree))))))


;; as is parsing one.

(defgrammar binary-tree
    :terminals ((label   "[^(),][^(),]*"))
    :start tree
    :rules ((--> tree
                 (opt node)
                 :action (if (null $1)
                             (make-empty-binary-tree)
                             $1)) ; it's identity, but make-empty-binary-tree
                                        ; could be defined otherwise.
            (--> node
                 label (opt children)
                 :action (make-binary-tree :label (read-from-string (second $1))
                                           :left (first $2) :right (second $2)))
            (--> children
                 "(" tree "," tree ")"
                 :action (list $2 $4))))


(defun binary-tree-from-string (string)
  (parse-binary-tree string))

;; (binary-tree-to-string (binary-tree-from-string "a(b(d,e),c(,f(g,)))"))
;; --> "A(B(D,E),C(,F(G,)))"



;; Solution p67 b):

;; There's no point to do something like this in lisp.  Difference lists
;; are a useful trick in prolog to be used with unification, but
;; generating the string from a dlist is nothing more than what we've
;; done so far:
;; 
;; (defun list-to-string (tree)
;;   (cond
;;     ((null tree)
;;      "")
;;     ;; Since the wanted syntax is irregular,
;;     ;; [it wants  a(b,c) instead of a(b(,),c(,))],
;;     ;; we need to add this special case:
;;     ((and (null (second tree))
;;           (null (third  tree)))
;;      (prin1-to-string (first tree)))
;;     (t
;;      (format nil "~A(~A,~A)"
;;              (first tree)
;;              (binary-tree-to-string (second tree))
;;              (binary-tree-to-string (third  tree))))))
;;
;; And of course, converting our binary trees to list is a No-Op, since
;; our trees are already implemented as lists.

;;;; THE END ;;;;
