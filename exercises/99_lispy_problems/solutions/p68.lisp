#-(and) "

 P68 (**) Preorder and inorder sequences of binary trees

    We consider binary trees with nodes that are identified by single
    lower-case letters, as in the example of problem P67.
   
    a) Write predicates preorder/2 and inorder/2 that construct the
    preorder and inorder sequence of a given binary tree,
    respectively. The results should be atoms, e.g. 'abdecfg' for the
    preorder sequence of the example in problem P67.
   
    b) Can you use preorder/2 from problem part a) in the reverse
    direction; i.e. given a preorder sequence, construct a
    corresponding tree? If not, make the necessary arrangements.
   
    c) If both the preorder sequence and the inorder sequence of the
    nodes of a binary tree are given, then the tree is determined
    unambiguously. Write a predicate pre-in-tree/3 that does the job.
   
    d) Solve problems a) to c) using difference lists. Cool! Use the
    predefined predicate time/1 to compare the solutions.
   
    What happens if the same character appears in more than one
    node. Try for instance pre-in-tree(aba,baa,T).
   
"


;;; Solution p68 a):


(defun inorder (tree)
  (labels ((inorder-string (tree)
             (if (binary-tree-empty-p tree)
                 ""
                 (format nil "~A~A~A"
                         (inorder-string (binary-tree-left tree))
                         (binary-tree-label tree)
                         (inorder-string (binary-tree-right tree))))))
    (intern (inorder-string tree))))


(defun preorder (tree)
  (labels ((preorder-string (tree)
             (if (binary-tree-empty-p tree)
                 ""
                 (format nil "~A~A~A" (binary-tree-label tree)
                         (preorder-string (binary-tree-left tree))
                         (preorder-string (binary-tree-right tree))))))
    (intern (preorder-string tree))))

;; (inorder '(A (B (D NIL NIL) (E NIL NIL)) (C NIL (F (G NIL NIL) NIL))))
;; --> DBEACGF
;; 
;; (preorder '(A (B (D NIL NIL) (E NIL NIL)) (C NIL (F (G NIL NIL) NIL))))
;; --> ABDECFG





;;; Solution p68 b):

;; It is not possible because there are different trees having the same preorder:
;;
;; (eql (preorder '(A (B (C (D nil nil) NIL) nil) nil))
;;      (preorder '(A nil (B nil (C nil (D nil nil)))))) --> T
;;
;; We can change this by generating a token for an empty tree, eg. a dash.



(defun full-preorder (tree)
  (labels ((preorder-string (tree)
             (if (binary-tree-empty-p tree)
                 "."
                 (format nil "~A~A~A" (binary-tree-label tree)
                         (preorder-string (binary-tree-left tree))
                         (preorder-string (binary-tree-right tree))))))
    (intern (preorder-string tree))))

;; (full-preorder '(A (B (D NIL NIL) (E NIL NIL)) (C NIL (F (G NIL NIL) NIL))))
;; --> ABD..E..C.FG...
;; 
;; (full-preorder '(A (B (C (D nil nil) NIL) nil) nil))  --> ABCD.....
;; (full-preorder '(A nil (B nil (C nil (D nil nil)))))  --> A.B.C.D..

;; Then we can write the inverse function:

(defun binary-tree-from-full-preorder (preorder)
  (labels ((inverse (stream)
             (let ((ch (read-char stream)))
               (if (char= #\. ch)
                   (make-empty-binary-tree)
                   (make-binary-tree :label (intern (string ch))
                                     :left  (inverse stream)
                                     :right (inverse stream))))))
    (with-input-from-string (stream (string preorder))
      (inverse stream))))


(dolist (tree '((a (b (c (d nil nil) NIL) nil) nil)
                (A nil (B nil (C nil (D nil nil))))
                (A (B (D NIL NIL) (E NIL NIL)) (C NIL (F (G NIL NIL) NIL)))))
  (assert (equalp tree (binary-tree-from-full-preorder (full-preorder tree)))))




;;; Solution p68 c):


(defun split-list (separator list)
  (loop
     :until (or (endp list) (eql separator (first list)))
     :collect (pop list) :into left
     :finally (return (values left (rest list)))))


;; We use the pre-order list to split the in-order list into the
;; in-order of the left subtree and of the right subtree, and
;; recursively.

(defun pre-in-order-lists (pre in)
  "
PRE:    the list of node labels in pre-order.
IN:     the list of node labels in in-order.
RETURN: the tree and the rest of PRE.
"
  (if (endp in)
      (values (make-empty-binary-tree)
              pre)
      (multiple-value-bind (left-in right-in) (split-list (first pre) in)
        (multiple-value-bind (left rest-of-pre) (pre-in-order-lists (rest pre) left-in)
          (multiple-value-bind (right rest-of-pre) (pre-in-order-lists rest-of-pre right-in)
            (values (make-binary-tree :label (first pre)
                                      :left  left
                                      :right right)
                    rest-of-pre))))))


(defun pre-in-tree (pre in)
  (pre-in-order-lists (map 'list (lambda (x) (intern (string x))) (string pre))
                      (map 'list (lambda (x) (intern (string x))) (string in))))


(dolist (tree '((a (b (c (d nil nil) NIL) nil) nil)
                (A nil (B nil (C nil (D nil nil))))
                (A (B (D NIL NIL) (E NIL NIL)) (C NIL (F (G NIL NIL) NIL)))
                (A (B NIL NIL) (A NIL NIL))
                (A NIL (A NIL (B NIL NIL)))))
  (assert (equalp tree (pre-in-tree (preorder tree) (inorder tree)))
          () "Tree = ~S~%preorder = ~S~%inorder  = ~S~% pre-in-tree ~S~%"
          tree (preorder tree) (inorder tree)
           (pre-in-tree (preorder tree) (inorder tree))))


;; With our lisp function, there's no problem to evaluate:
;; (pre-in-order 'aba 'baa)
;; --> (A (B NIL NIL) (A NIL NIL))
;;
;; On the other hand, both (A NIL (A NIL (B NIL NIL))) and (A (A NIL NIL) (B NIL NIL))
;; have the same pre and in order:
;;
;; (mapcar (lambda (tree) (list (preorder tree) (inorder tree)))
;;         '((A NIL (A NIL (B NIL NIL)))
;;           (A (A NIL NIL) (B NIL NIL))))
;; --> ((AAB AAB) (AAB AAB))


;;;; THE END ;;;;

