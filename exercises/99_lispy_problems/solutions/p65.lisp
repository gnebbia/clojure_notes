#+(and) "
P65 (**) Layout a binary tree (2)
    [p65]   

    An alternative layout method is depicted in the illustration
    opposite. Find out the rules and write the corresponding Prolog
    predicate. Hint: On a given level, the horizontal distance between
    neighboring nodes is constant.
   
    Use the same conventions as in problem P64 and test your predicate in an appropriate way.

"


;; The rule seems to be that the ordinate is the depth of the node,
;; and the abscissa of a node is offset from the parent by 2^height of the node.
;; 
;; The abscissa of the leftmost node is fixed to 1,
;; and the ordinate of the root is fixed to 1.
;; 
;; height of the tree = depth of node + height of node.


(load "p54a")



(defun binary-tree-count-leftmosts (tree)
  (if (binary-tree-empty-p tree)
      0
      (+ 1 (binary-tree-count-leftmosts (binary-tree-left tree)))))


(defun layout-node-p65 (node abscissa depth height)
  "
The abscissa of the NODE is given by ABSCISSA, and the ordinate by DEPTH.
The abscissa of the children is offset by (expt 2 height).
"
  (setf (layout-binary-tree-x node) abscissa
        (layout-binary-tree-y node) depth)
  (let* ((height-1 (1- height))
         (depth+1  (1+ depth))
         (offset   (expt 2 height-1)))
    (unless (binary-tree-empty-p (binary-tree-left node))
      (layout-node-p65 (binary-tree-left node)
                       (- abscissa offset)
                       depth+1
                       height-1))
    (unless (binary-tree-empty-p (binary-tree-right node))
      (layout-node-p65 (binary-tree-right node)
                       (+ abscissa offset)
                       depth+1
                       height-1)))
  node)


(defun layout-binary-tree-p65 (tree)
  (let ((height (binary-tree-height tree)))
    (layout-node-p65 (binary-tree-to-layout-binary-tree tree)
                     (1+ (- (expt 2 (1- height))
                            (expt 2 (- height (binary-tree-count-leftmosts tree)))))
                     1
                     (1- height))))


(assert (= 4 (binary-tree-count-leftmosts (construct '(n k c a e d g m u p q) (function string<)))))
(assert (= 5 (binary-tree-height          (construct '(n k c a e d g m u p q) (function string<)))))
(assert (equal (layout-binary-tree-p65    (construct '(n k c a e d g m u p q) (function string<)))
               '(N (K (C (A NIL NIL 1 4)
                       (E (D NIL NIL 4 5)
                          (G NIL NIL 6 5) 5 4) 3 3)
                    (M NIL NIL 11 3) 7 2)
                 (U (P NIL
                       (Q NIL NIL 21 4) 19 3)
                    NIL 23 2) 15 1)))


;; (list
;;  (draw-laid-out-tree (layout-binary-tree-p65 (complete-binary-tree 7)))
;;  (draw-laid-out-tree (layout-binary-tree-p65 (construct '(n k c a e d g m u p q)   (function string<))))
;;  (draw-laid-out-tree (layout-binary-tree-p65 (construct '(n k c a h g e m u p s q) (function string<)))))
;; 
;; (                                
;;         1                       
;;        / \                      
;;     2       3                   
;;    / \     / \                  
;;   4   5   6   7                 
;; 
;;                                                                                                  
;;                               N                                                                 
;;                              / \                                                                
;;               K                               U                                                 
;;              / \                             /                                                  
;;       C               M               P                                                         
;;      / \                               \                                                        
;;   A       E                               Q                                                     
;;          / \                                                                                    
;;         D   G                                                                                   
;; 
;;                                                                                                                                                                                          
;;                                                           N                                                                                                                             
;;                                                          / \                                                                                                                            
;;                           K                                                               U                                                                                             
;;                          / \                                                             /                                                                                              
;;           C                               M                               P                                                                                                             
;;          / \                                                               \                                                                                                            
;;   A               H                                                               S                                                                                                     
;;                  /                                                               /                                                                                                      
;;               G                                                               Q                                                                                                         
;;              /                                                                                                                                                                          
;;             E                                                                                                                                                                           
;; )

;;;; THE END ;;;;
