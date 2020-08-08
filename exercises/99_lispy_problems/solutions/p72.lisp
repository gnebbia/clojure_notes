#-(and) "

P72 (*) Construct the bottom-up order sequence of the tree nodes

   Write a predicate bottom-up(Tree,Seq) which constructs the
   bottom-up sequence of the nodes of the multiway tree Tree. Seq
   should be a Prolog list. What happens if you run your predicate
   backwords?
"

;; "Bottom-up order sequence of tree nodes" is an idiosyncrasy
;; (google for it, there's no definition!).
;; Perhaps it means postfix order.
;; Right, it's the postfix order.  The prolog solution gives:
;; ?- bottom_up(t(a,[t(f,[t(g,[])]),t(c,[]),t(b,[t(d,[]),t(e,[])])]),L).
;; L = [g, f, c, d, e, b, a].


(defun multiway-tree-postfix-order (tree)
  "
Returns a list of node labels in the postfix order.
"
  (reduce (function nconc)
          (multiway-tree-children tree)
          :key (function multiway-tree-postfix-order)
          :initial-value (list (multiway-tree-label tree))
          :from-end t))
;; :from-end is needed so that the initial-value is placed on the right.
;; It also proves beneficial since then the lists are walked only once per level.

;; (multiway-tree-postfix-order (parse-multiway-tree-string "AFG^^C^BD^E^^^"))
;; --> (G F C D E B A)  


(defun bottom-up (tree)
  (multiway-tree-postfix-order tree))


;;;; THE END ;;;;
