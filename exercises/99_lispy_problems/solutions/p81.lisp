#-(and) "
P81 (**) Path from one node to another one

    Write a predicate path(G,A,B,P) to find an acyclic path P from
    node A to node b in the graph G. The predicate should return all
    paths via backtracking.
"

(load "p80.lisp")


(defmethod path ((g graph) src dst)
  "Return a list of all the paths going from SRC to DST.
We don't include paths containing cycles, since there would be an
infinite number of them… "
  (labels ((walk (path)
             (mapcan (lambda (arc)
                       (cond
                         ((eql (arc-to arc) dst)
                          (list (cons dst path)))
                         ((member (arc-to arc) path) ; cycle
                          '())
                         (t
                          (walk (cons (arc-to arc) path)))))
                     (arcs-from-node g (first path)))))
    (mapcar 'reverse (walk (list src)))))


;; (dolist (test '(
;;                 (a c (a b c))
;;                 (a c ((a b) (b c)))
;;                 (b g ((b c) (f c) (g h) d (f b) (k f) (h g)))
;;                 (s u ((s r) t (u r) (s u) (u s) (v u)))
;;                 (p m ((p q :weight 9) (m q :weight 7) k (p m :weight 5))))
;;          (terpri))
;;   (print (list test (path (make-adjacency-list-graph (third test)) (first test) (second test)))))
;; 
;; ((A C (A B C)) NIL) 
;; ((A C ((A B) (B C))) ((A B C))) 
;; ((B G ((B C) (F C) (G H) D (F B) (K F) (H G))) NIL) 
;; ((S U ((S R) T (U R) (S U) (U S) (V U))) ((S U))) 
;; ((P M ((P Q :WEIGHT 9) (M Q :WEIGHT 7) K (P M :WEIGHT 5))) ((P M)))
;; NIL
;;
;; 
;; (dolist (test '((a c ((a b) (b c)))
;;                 (b g ((b c) (f c) (g h) (f b) (k f) (h g)))
;;                 (s u ((s r) (u r) (s u) (u s) (v u)))
;;                 (p m ((p q :weight 9) (m q :weight 7) (p m :weight 5))))
;;          (terpri))
;;   (print (list test (path (make-edge-graph (third test)) (first test) (second test)))))
;; 
;; 
;; ((A C ((A B) (B C))) ((A B C))) 
;; ((B G ((B C) (F C) (G H) (F B) (K F) (H G))) NIL) 
;; ((S U ((S R) (U R) (S U) (U S) (V U))) ((S U) (S U) (S R U))) 
;; ((P M ((P Q :WEIGHT 9) (M Q :WEIGHT 7) (P M :WEIGHT 5))) ((P M) (P Q M))) 
;; NIL


;;;; THE END ;;;;
