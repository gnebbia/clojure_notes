#-(and) "
   
P64 (**) Layout a binary tree (1)

    Given a binary tree as the usual Prolog term t(X,L,R) (or nil). As
    a preparation for drawing the tree, a layout algorithm is required
    to determine the position of each node in a rectangular
    grid. Several layout methods are conceivable, one of them is shown
    in the illustration below.
   
    [p64]   
    In this layout strategy, the position of a node v is obtained by the following two rules:
   
      □ x(v) is equal to the position of the node v in the inorder sequence
      □ y(v) is equal to the depth of the node v in the tree

    In order to store the position of the nodes, we extend the Prolog
    term representing a node (and its successors) as follows:
   
    % nil represents the empty tree (as usual)
    % t(W,X,Y,L,R) represents a (non-empty) binary tree with root W
    %  \"positioned\" at (X,Y), and subtrees L and R
   
    Write a predicate layout-binary-tree/2 with the following specification:
   
    % layout-binary-tree(T,PT) :- PT is the \"positioned\" binary tree
    % obtained from the binary tree T. (+,?)
   
    Test your predicate in an appropriate way.
"

(load "p54a")


;; To add the coordinates, we create a new structure, which inherits
;; from the binary-tree structure, so we can reuse that abstraction.
;; However, including structures will make the new fields added at the
;; end of it.  The order of the fields should be immaterial (only that
;; we don't use true structures, but lists, so the new fields are
;; added at the end of the lists, compared to the included list
;; structures).

(defstruct (layout-binary-tree (:include binary-tree)
                               (:type list))
  x y)



(defun binary-tree-to-layout-binary-tree (tree)
  "
Return a layout-binary-tree homologue to node.
"
  (if (binary-tree-empty-p tree)
      (make-empty-binary-tree)
      (make-layout-binary-tree
       :label (binary-tree-label tree)
       :left  (binary-tree-to-layout-binary-tree (binary-tree-left  tree))
       :right (binary-tree-to-layout-binary-tree (binary-tree-right tree)))))


;; To layout the binary tree, we will do it in two steps.   First we
;; make the layout tree, and setting the y field to the depth of each
;; node.  Then we execute a infix walk of the new tree updating the x
;; field of each node.

(defun layout-node-depth (node depth)
  "
Return a layout-binary-tree homologue to node, with the ordinates of
each node set to their depth.
"
  (if (binary-tree-empty-p node)
      (make-empty-binary-tree)
      (make-layout-binary-tree
       :label (binary-tree-label node)
       :y depth
       :left  (layout-node-depth (binary-tree-left  node) (1+ depth))
       :right (layout-node-depth (binary-tree-right node) (1+ depth)))))



;; Note, incf is a prefix increment, it returns the new-value.
;; Therefore it is easier to start with the predecessor of the first
;; value, and to finally return the last value used.  One could define
;; a postfix increment operator to easily write the code using the
;; other convention.

(defun layout-node-abscissa/inorder (node abscissa)
  "
Sets the abscissa of each node in the subtree NODE to a sequence of
values starting from (1+ ABSCISSA) for the left-most node.
Returns the last abscissa used.
"
  (when (binary-tree-left node)
    (setf abscissa (layout-node-abscissa/inorder (binary-tree-left node) abscissa)))
  (setf (layout-binary-tree-x node) (incf abscissa))
  (when (binary-tree-right node)
    (setf abscissa (layout-node-abscissa/inorder (binary-tree-right node) abscissa)))
  abscissa)


(defun layout-binary-tree-p64 (tree)
  (let ((lobt (layout-node-depth tree 1)))
    (layout-node-abscissa/inorder lobt 0) ; starts from 1; use -1 to start from 0.
    lobt))



(defun binary-tree-rightmost-node (tree)
  (unless (binary-tree-empty-p tree)
    (if (binary-tree-empty-p (binary-tree-right tree))
        tree
        (binary-tree-rightmost-node (binary-tree-right tree)))))


(defun draw-laid-out-node (node picture)
  (let* ((label  (princ-to-string (binary-tree-label node)))
         (lab    (case (length label)
                   ((0) " . ")
                   ((1) (format nil " ~A " label))
                   ((2) (format nil " ~A"  label))
                   ((3) label)
                   (otherwise (subseq label 0 3))))
         (height (com.informatimago.common-lisp.picture.picture:height picture))
         (2x     (* 2 (layout-binary-tree-x node)))
         (2y     (- height (* 2 (layout-binary-tree-y node)))))
    (com.informatimago.common-lisp.picture.picture:draw-string
     picture (1- 2x) 2y lab)
    (when (binary-tree-left node)
      (com.informatimago.common-lisp.picture.picture:draw-string
       picture (1- 2x) (1- 2y) "/")
      (draw-laid-out-node (binary-tree-left node) picture))
    (when (binary-tree-right node)
      (com.informatimago.common-lisp.picture.picture:draw-string
       picture (1+ 2x) (1- 2y) "\\")
      (draw-laid-out-node (binary-tree-right node) picture))
    picture))


(defun draw-laid-out-tree (tree)
  (let* ((height    (* 2 (binary-tree-height tree)))
         (rightmost (binary-tree-rightmost-node tree))
         (width     (* 4 (1+ (layout-binary-tree-x rightmost))))
         ;;   N  
         ;;  / \
         ;; K   U
         (picture   (make-instance 'com.informatimago.common-lisp.picture.picture:picture
                        :width width :height height)))
    (draw-laid-out-node tree picture)))


(assert (equal (layout-binary-tree-p64  (complete-binary-tree 7))
               '(1
                 (2 (4 NIL NIL 1 3) (5 NIL NIL 3 3) 2 2)
                 (3 (6 NIL NIL 5 3) (7 NIL NIL 7 3) 6 2)
                 4 1)))

(assert (equal (layout-binary-tree-p64   (construct '(n k c a h g e m u p s q) (function string<)))
               '(N (K (C (A NIL NIL 1 4)
                       (H (G (E NIL NIL 3 6)
                             NIL 4 5)
                          NIL 5 4)
                       2 3)
                    (M NIL NIL 7 3)
                    6 2)
                 (U (P NIL
                       (S (Q NIL NIL 10 5)
                          NIL 11 4)
                       9 3)
                    NIL 12 2)
                 8 1)))

(assert (equal (layout-binary-tree-p64   (construct '(n k c a e d g m u p q) (function string<)))
               '(N (K (C (A NIL NIL 1 4)
                       (E (D NIL NIL 3 5)
                          (G NIL NIL 5 5)
                          4 4)
                       2 3)
                    (M NIL NIL 7 3)
                    6 2)
                 (U (P NIL
                       (Q NIL NIL 10 4)
                       9 3)
                    NIL 11 2)
                 8 1)))

;; (list
;;  (draw-laid-out-tree (layout-binary-tree-p64 (complete-binary-tree 7)))
;;  (draw-laid-out-tree (layout-binary-tree-p64 (construct '(n k c a e d g m u p q)   (function string<))))
;;  (draw-laid-out-tree (layout-binary-tree-p64 (construct '(n k c a h g e m u p s q) (function string<)))))
;; 
;; (                                
;;         1                       
;;        / \                      
;;     2       3                   
;;    / \     / \                  
;;   4   5   6   7                 
;; 
;;                                                  
;;                 N                               
;;                / \                              
;;             K         U                         
;;            / \       /                          
;;     C         M   P                             
;;    / \             \                            
;;   A     E           Q                           
;;        / \                                      
;;       D   G                                     
;; 
;;                                                      
;;                 N                                   
;;                / \                                  
;;             K           U                           
;;            / \         /                            
;;     C         M   P                                 
;;    / \             \                                
;;   A       H           S                             
;;          /           /                              
;;         G           Q                               
;;        /                                            
;;       E                                             
;; )


;;;; THE END ;;;;

