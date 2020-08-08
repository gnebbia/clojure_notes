#-(and) "

P66 (***) Layout a binary tree (3) 

    Yet another layout strategy is shown in the illustration
    opposite. The method yields a very compact layout while
    maintaining a certain symmetry in every node. Find out the rules
    and write the corresponding Prolog predicate. Hint:

    Consider the horizontal distance between a node and its successor
    nodes. How tight can you pack together two subtrees to construct
    the combined binary tree?

    Use the same conventions as in problem P64 and P65 and test your
    predicate in an appropriate way. Note: This is a difficult
    problem. Don't give up too early!

    Which layout do you like most?

"


;; The rule seems to be that the ordinate is the depth of the node,
;; and the abscissa of a node is offset from the parent by at least
;; one unit, more to accumodate the width of the subtrees, with the
;; constraint that two nodes (possibly of different subtrees) be
;; spaced by two units.
;; 
;; The abscissa of the leftmost node is fixed to 1,
;; and the ordinate of the root is fixed to 1.
;; 
;; height of the tree = depth of node + height of node.




;; to let subtrees come as close the possible one to the other, we
;; will keep the left and right offsets for all levels below the node.
;; The offsets are kept in a list from the child down to the leaves,
;; relative to the node abscissa.

(defstruct (jagged-layout-binary-tree (:include layout-binary-tree)
                                      (:type list))
  left-offsets right-offsets)



(defun jag-layout-binary-tree (tree depth)
  "
Builds a jagged-layout-binary-tree homologue to TREE, with offsets
computed to let subtrees be as close as possible.
"
  (if (binary-tree-empty-p tree)
      (make-empty-binary-tree)
      (if (binary-tree-empty-p (binary-tree-left tree))
          (if (binary-tree-empty-p (binary-tree-right tree))
              ;; leaf
              (make-jagged-layout-binary-tree
               :label (binary-tree-label tree)
               :x 0
               :y depth
               :left-offsets '()
               :right-offsets '())
              ;; only right child.
              (let ((right (jag-layout-binary-tree (binary-tree-right tree) (1+ depth))))
                (make-jagged-layout-binary-tree
                 :label (binary-tree-label tree)
                 :x 0
                 :y depth
                 :right right
                 :left-offsets  (cons 1 (mapcar (function 1+) (jagged-layout-binary-tree-left-offsets  right)))
                 :right-offsets (cons 1 (mapcar (function 1+) (jagged-layout-binary-tree-right-offsets right))))))
          (if (binary-tree-empty-p (binary-tree-right tree))
              ;; only left child.
              (let ((left (jag-layout-binary-tree (binary-tree-left tree) (1+ depth))))
                (make-jagged-layout-binary-tree
                 :label (binary-tree-label tree)
                 :x 0
                 :y depth
                 :left left
                 :left-offsets  (cons -1 (mapcar (function 1-) (jagged-layout-binary-tree-left-offsets  left)))
                 :right-offsets (cons -1 (mapcar (function 1-) (jagged-layout-binary-tree-right-offsets left)))))
              ;; both left and right children.
              (let* ((left  (jag-layout-binary-tree (binary-tree-left  tree) (1+ depth)))
                     (right (jag-layout-binary-tree (binary-tree-right tree) (1+ depth)))
                     ;; 0         0                 
                     ;;  \       /
                     ;; right 2 left
                     (offset (/ (+ 2 (reduce (function max)
                                             (mapcar (function -)
                                                     (jagged-layout-binary-tree-right-offsets left)
                                                     (jagged-layout-binary-tree-left-offsets  right))
                                             :initial-value 0))
                                2)))
                (make-jagged-layout-binary-tree
                 :label (binary-tree-label tree)
                 :x 0
                 :y depth
                 :left left
                 :right right
                 :left-offsets  (cons (- offset) (mapcar (lambda (x) (- x offset))
                                                         (jagged-layout-binary-tree-left-offsets  left)))
                 :right-offsets (cons (+ offset) (mapcar (lambda (x) (+ x offset))
                                                         (jagged-layout-binary-tree-right-offsets right)))))))))


(defun jagged-layout-binary-tree-complete-layout (tree offset)
  (unless (binary-tree-empty-p tree)
    (incf (layout-binary-tree-x tree) offset)
    (when (binary-tree-left tree)
      (jagged-layout-binary-tree-complete-layout
       (binary-tree-left tree)
       (+ offset (car (jagged-layout-binary-tree-left-offsets  tree)))))
    (when (binary-tree-right tree)
      (jagged-layout-binary-tree-complete-layout
       (binary-tree-right tree)
       (+ offset (car (jagged-layout-binary-tree-right-offsets tree)))))
    tree))



(defun layout-binary-tree-p66 (tree)
  (let ((jtree (jag-layout-binary-tree tree 1)))
    (jagged-layout-binary-tree-complete-layout jtree (- (reduce (function min)
                                                                (jagged-layout-binary-tree-left-offsets jtree))))
    jtree))


;; (list
;;  (draw-laid-out-tree (layout-binary-tree-p66 (complete-binary-tree 7)))
;;  (draw-laid-out-tree (layout-binary-tree-p66 (construct '(n k c a e d g m u p q)   (function string<))))
;;  (draw-laid-out-tree (layout-binary-tree-p66 (construct '(n k c a h g e m u p s q) (function string<)))))
;; 
;; (                            
;;       1                     
;;      / \                    
;;   2       3                 
;;  / \     / \                
;; 4   5   6   7               
;; 
;;                              
;;         N                   
;;        / \                  
;;     K       U               
;;    / \     /                
;;   C   M   P                 
;;  / \       \                
;; A   E       Q               
;;    / \                      
;;   D   G                     
;; 
;;                              
;;         N                   
;;        / \                  
;;     K       U               
;;    / \     /                
;;   C   M   P                 
;;  / \       \                
;; A   H       S               
;;    /       /                
;;   G       Q                 
;;  /                          
;; E                           
;; )



;; I prefer draw-tree, which uses nice unicode characters, and rotates
;; the tree 90 degree so that it can draw wider trees.  :-)
;;
;; (draw-tree (construct '(n k c a h g e m u p s q) (function string<)))
;;                                                      
;;                                                      
;;         ┌─── nil                                     
;;       ╔═╧═╗                                          
;;   ┌───╢ U ║                                          
;;   │   ╚═╤═╝                                          
;;   │     │           ┌─── nil                         
;;   │     │         ╔═╧═╗                              
;;   │     │     ┌───╢ S ║                              
;;   │     │     │   ╚═╤═╝   ┌─── nil                   
;;   │     │     │     │   ╔═╧═╗                        
;;   │     │     │     └───╢ Q ║                        
;;   │     │     │         ╚═╤═╝                        
;;   │     │     │           └─── nil                   
;;   │     │   ╔═╧═╗                                    
;;   │     └───╢ P ║                                    
;;   │         ╚═╤═╝                                    
;;   │           └─── nil                               
;; ╔═╧═╗                                                
;; ╢ N ║                                                
;; ╚═╤═╝                                                
;;   │           ┌─── nil                               
;;   │         ╔═╧═╗                                    
;;   │     ┌───╢ M ║                                    
;;   │     │   ╚═╤═╝                                    
;;   │   ╔═╧═╗   └─── nil                               
;;   └───╢ K ║                                          
;;       ╚═╤═╝                                          
;;         │           ┌─── nil                         
;;         │         ╔═╧═╗                              
;;         │     ┌───╢ H ║                              
;;         │     │   ╚═╤═╝   ┌─── nil                   
;;         │     │     │   ╔═╧═╗                        
;;         │     │     └───╢ G ║                        
;;         │     │         ╚═╤═╝   ┌─── nil             
;;         │     │           │   ╔═╧═╗                  
;;         │     │           └───╢ E ║                  
;;         │     │               ╚═╤═╝                  
;;         │     │                 └─── nil             
;;         │     │                                      
;;         │   ╔═╧═╗                                    
;;         └───╢ C ║                                    
;;             ╚═╤═╝   ┌─── nil                         
;;               │   ╔═╧═╗                              
;;               └───╢ A ║                              
;;                   ╚═╤═╝                              
;;                     └─── nil                         
                                                     
                                                     
                                                     
;; No, really p66 is nice a compact layout, but it has the problem
;; that it doesn't accomodate wider labels (and neither do p64, and
;; p65).  Since draw-tree draws the labels perpendicularly, it doesn't
;; have any problem with longer labels .


;;;; THE END ;;;;
