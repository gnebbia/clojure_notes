#-(and) "

P58 (**) Generate-and-test paradigm

    Apply the generate-and-test paradigm to construct all symmetric,
    completely balanced binary trees with a given number of
    nodes. Example:

    * sym-cbal-trees(5,Ts).

    Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)),
    t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))]
   
    How many such trees are there with 57 nodes? Investigate about how
    many solutions there are for a given number of nodes? What if the
    number is even? Write an appropriate predicate.
"

(load "p54a")
(load "p55")
(load "p56")
(load "p57")

(load "memoize")
(use-package :org.tfeb.hax.memoize)
(import 'alexandria:nconcf)

(load "draw-tree") ; to dump trees in nice ASCII-ART.



(defun generate-balanced-binary-trees-of-height (height next-label)
  ;; When next-label is a constant function,
  ;; generate-balanced-binary-trees-of-height could be memoized.
  (case height
    (0 (list (make-empty-binary-tree)))
    (1 (list (make-binary-tree :label (funcall next-label))))
    (otherwise
     (let ((h-2 (generate-balanced-binary-trees-of-height (- height 2) next-label))
           (h-1 (generate-balanced-binary-trees-of-height (- height 1) next-label)))
       (nconc
        (mapcan (lambda (left)
                  (mapcar (lambda (right)
                            (make-binary-tree :label (funcall next-label)
                                              :left left
                                              :right right))
                          h-1))
                h-1)
        (mapcan (lambda (left)
                  (mapcar (lambda (right)
                            (make-binary-tree :label (funcall next-label)
                                              :left left
                                              :right right))
                          h-1))
                h-2)
        (mapcan (lambda (right)
                  (mapcar (lambda (left)
                            (make-binary-tree :label (funcall next-label)
                                              :left left
                                              :right right))
                          h-1))
                h-2))))))

(memoize-function 'generate-balanced-binary-trees-of-height)




;; The following function generates all the binary trees with a given
;; number of nodes.  Even when memoized, this function is still O(e^n).
;;
;; It can be used to filter the wanted trees, but only with a number
;; of node much less than 57: for 17 nodes it takes on the order of
;; 10 minutes, much more for 18 nodes.

(defun generate-binary-trees-of-nodes (number-of-nodes next-label)
  ;; When next-label is a constant function,
  ;; generate-binary-trees-of-nodes could be memoized.
  (case number-of-nodes
    (0 (list (make-empty-binary-tree)))
    (1 (list (make-binary-tree :label (funcall next-label))))
    (otherwise
     (let ((subtrees (loop
                        :with subtrees = (make-array number-of-nodes)
                        :for i :from 0 :below number-of-nodes
                        :do (setf (aref subtrees i) (generate-binary-trees-of-nodes i next-label))
                        :finally (return subtrees))))
       (loop
          :for left :from 0 :below number-of-nodes
          :for right = (- number-of-nodes left 1)
          :nconc (mapcan (lambda (left-subtree)
                           (mapcar (lambda (right-subtree)
                                     (make-binary-tree :label (funcall next-label)
                                                       :left left-subtree
                                                       :right right-subtree))
                                   (aref subtrees right)))
                         (aref subtrees left)))))))

(memoize-function 'generate-binary-trees-of-nodes)

(defun sym-cbal-trees (n)
  (remove-if-not (lambda (tree)
                   (and (binary-tree-symetric-p tree)
                        (binary-tree-balanced-p tree)))
                 (generate-binary-trees-of-nodes n (constantly 'x))))





#-(and)
(mapcar 'draw-tree
        (generate-binary-trees-of-nodes 5 (let ((n 0)) (lambda () (incf n)))))

#-(and)
(progn
  (defparameter *box*  *ascii-box*)
  (defparameter *line* *ascii-line*))
(progn
  (defparameter *box*  *unicode-box*)
  (defparameter *line* *unicode-line*))

#-(and) "
CL-USER> (mapcar 'draw-tree
                 (generate-binary-trees-of-nodes 3 (let ((n 0)) (lambda () (incf n)))))
(                             
                             
                             
              ┌─── nil       
            ╔═╧═╗            
        ┌───╢ 2 ║            
        │   ╚═╤═╝            
      ╔═╧═╗   └─── nil       
  ┌───╢ 3 ║                  
  │   ╚═╤═╝                  
╔═╧═╗   └─── nil             
╢ 5 ║                        
╚═╤═╝                        
  └─── nil                   

                              
                             
        ┌─── nil             
      ╔═╧═╗                  
  ┌───╢ 4 ║                  
  │   ╚═╤═╝   ┌─── nil       
  │     │   ╔═╧═╗            
  │     └───╢ 2 ║            
  │         ╚═╤═╝            
  │           └─── nil       
╔═╧═╗                        
╢ 6 ║                        
╚═╤═╝                        
  └─── nil                   

                      
                     
        ┌─── nil     
      ╔═╧═╗          
  ┌───╢ 1 ║          
  │   ╚═╤═╝          
╔═╧═╗   └─── nil     
╢ 7 ║                
╚═╤═╝   ┌─── nil     
  │   ╔═╧═╗          
  └───╢ 1 ║          
      ╚═╤═╝          
        └─── nil     
                     

                              
  ┌─── nil                   
╔═╧═╗                        
╢ 8 ║                        
╚═╤═╝                        
  │           ┌─── nil       
  │         ╔═╧═╗            
  │     ┌───╢ 2 ║            
  │     │   ╚═╤═╝            
  │   ╔═╧═╗   └─── nil       
  └───╢ 3 ║                  
      ╚═╤═╝                  
        └─── nil             
                             

                              
  ┌─── nil                   
╔═╧═╗                        
╢ 9 ║                        
╚═╤═╝   ┌─── nil             
  │   ╔═╧═╗                  
  └───╢ 4 ║                  
      ╚═╤═╝   ┌─── nil       
        │   ╔═╧═╗            
        └───╢ 2 ║            
            ╚═╤═╝            
              └─── nil       
                             
                             
)
"


;; (loop for i below 18 collect (cons i  (length (generate-binary-trees-of-nodes i (constantly 'x)))))
;; ((0 . 1)
;;  (1 . 1)
;;  (2 . 2)
;;  (3 . 5)
;;  (4 . 14)
;;  (5 . 42)
;;  (6 . 132)
;;  (7 . 429)
;;  (8 . 1430)
;;  (9 . 4862)
;;  (10 . 16796)
;;  (11 . 58786)
;;  (12 . 208012)
;;  (13 . 742900)
;;  (14 . 2674440)
;;  (15 . 9694845)
;;  (16 . 35357670)
;;  (17 . 129644790))




#-(and)"


CL-USER> (mapcar 'draw-tree (sym-cbal-trees 5))
(                             
                             
        ┌─── nil             
      ╔═╧═╗                  
  ┌───╢ X ║                  
  │   ╚═╤═╝   ┌─── nil       
  │     │   ╔═╧═╗            
  │     └───╢ X ║            
  │         ╚═╤═╝            
  │           └─── nil       
╔═╧═╗                        
╢ X ║                        
╚═╤═╝                        
  │           ┌─── nil       
  │         ╔═╧═╗            
  │     ┌───╢ X ║            
  │     │   ╚═╤═╝            
  │   ╔═╧═╗   └─── nil       
  └───╢ X ║                  
      ╚═╤═╝                  
        └─── nil             
                             

                              
                             
                             
              ┌─── nil       
            ╔═╧═╗            
        ┌───╢ X ║            
        │   ╚═╤═╝            
      ╔═╧═╗   └─── nil       
  ┌───╢ X ║                  
  │   ╚═╤═╝                  
╔═╧═╗   └─── nil             
╢ X ║                        
╚═╤═╝   ┌─── nil             
  │   ╔═╧═╗                  
  └───╢ X ║                  
      ╚═╤═╝   ┌─── nil       
        │   ╔═╧═╗            
        └───╢ X ║            
            ╚═╤═╝            
              └─── nil       
                             
                             
)
CL-USER>

"




(defun generate-balanced-binary-trees-of-height (height next-label)
  ;; When next-label is a constant function,
  ;; generate-balanced-binary-trees-of-height could be memoized.
  (case height
    (0 (list (make-empty-binary-tree)))
    (1 (list (make-binary-tree :label (funcall next-label))))
    (otherwise
     (let ((h-2 (generate-balanced-binary-trees-of-height (- height 2) next-label))
           (h-1 (generate-balanced-binary-trees-of-height (- height 1) next-label)))
       (nconc
        (mapcan (lambda (left)
                  (mapcar (lambda (right)
                            (make-binary-tree :label (funcall next-label)
                                              :left left
                                              :right right))
                          h-1))
                h-1)
        (mapcan (lambda (left)
                  (mapcar (lambda (right)
                            (make-binary-tree :label (funcall next-label)
                                              :left left
                                              :right right))
                          h-1))
                h-2)
        (mapcan (lambda (right)
                  (mapcar (lambda (left)
                            (make-binary-tree :label (funcall next-label)
                                              :left left
                                              :right right))
                          h-1))
                h-2))))))



(defun safe-aref (vector index)
  (and (<= 0 index (1- (length vector))) (aref vector index)))

(declaim (inline safe-aref))


 (defun generate-balanced-binary-trees-of-nodes (number-of-nodes next-label)
  "Returns all the balanced binary trees that have NUMBER-OF-NODES nodes,
in an vector indexed by the height.
"
  ;; When next-label is a constant function,
  ;; generate-balanced-binary-trees-of-nodes could be memoized.
  (case number-of-nodes
    (0 (vector (list (make-empty-binary-tree))))
    (1 (vector '() (list (make-binary-tree :label (funcall next-label)))))
    (otherwise

     ;; First we get the balanced binary trees with all the number of
     ;; nodes from 0 to (1- number-of-nodes).
     ;;
     ;; Then we will combine two of them each such that the sums of
     ;; number of nodes (plus one for the root) is number-of-nodes,
     ;; but taking care of the balancing.
     
     (let ((subtrees (loop
                        :with subtrees = (make-array number-of-nodes)
                        :for i :from 0 :below number-of-nodes
                        :do (setf (aref subtrees i)
                                  (generate-balanced-binary-trees-of-nodes i next-label))
                        :finally (return subtrees)))
           ;; The results will be collected in this vector:
           (byheight (make-array (1+ number-of-nodes) :initial-element '())))
       (loop
          :for left :from 0 :below number-of-nodes
          :for right = (- number-of-nodes left 1)

          :for left-byheight  = (aref subtrees left)
          :for right-byheight = (aref subtrees right)

          :do (assert (= (+ left right 1) number-of-nodes))

          ;; Note, we could be smarter here, since given a number of
          ;; nodes, there are minimum and maximum heights of tree you
          ;; can build with them.  For now, we just loop over the
          ;; heights, and check each time the existance of the
          ;; subtrees:
          
          :do (loop
                 :for left-height    :from 0 
                 :for left-subtrees  :across left-byheight
                 :when left-subtrees
                 ;; Find right-subtrees of about the same height as
                 ;; the left ones, and compute their cross product:
                 :do (let ((above (safe-aref right-byheight (1+ left-height)))
                           (level (safe-aref right-byheight     left-height))
                           (below (safe-aref right-byheight (1- left-height))))
                       (when (< (+ left-height 2) (length byheight))
                         (nconcf (aref byheight (+ left-height 2))
                                 (mapcan (lambda (right-subtree)
                                           (mapcar (lambda (left-subtree)
                                                     (make-binary-tree :label (funcall next-label)
                                                                       :left left-subtree
                                                                       :right right-subtree))
                                                   left-subtrees))
                                         above)))
                       (nconcf (aref byheight (+ left-height 1))
                                (mapcan (lambda (right-subtree)
                                          (mapcar (lambda (left-subtree)
                                                    (make-binary-tree :label (funcall next-label)
                                                                      :left left-subtree
                                                                      :right right-subtree))
                                                  left-subtrees))
                                        (append level below))))))
       byheight))))

(memoize-function 'generate-balanced-binary-trees-of-nodes)

;; (map nil (lambda (l) (print l) (mapcar (lambda (n) (princ (draw-tree n)) (terpri)) l))
;;      (generate-balanced-binary-trees-of-nodes 4 (constantly 'x)))

(defun sym-cbal-trees (n)
  (remove-if-not (lambda (tree)
                   (and (binary-tree-symetric-p tree)
                        (binary-tree-balanced-p tree)))
                 (coerce
                  (reduce (function nconc)
                          (generate-balanced-binary-trees-of-nodes n (constantly 'x)))
                  'list)))

;; (loop :for i :below 58  :collect (time (cons i (length (sym-cbal-trees i)))))




;;;; THE END ;;;;
