#-(and) "

P70 (**) Tree construction from a node string

    We suppose that the nodes of a multiway tree contain single
    characters. In the depth-first order sequence of its nodes, a
    special character ^ has been inserted whenever, during the tree
    traversal, the move is a backtrack to the previous level.
   
    By this rule, the tree in the figure opposite is represented as:
    afg^^c^bd^e^^^

          a
         /|\
        / | \
       f  c  b
       |    / \
       g   d   e
   
    Define the syntax of the string and write a predicate
    tree(String,Tree) to construct the Tree when the String is
    given. Work with atoms (instead of strings). Make your predicate
    work in both directions.
"

(load "p70c")
(load "rdp")

;; Solution: in lisp, we write two function, to parse and to generate.
;; Again parsing and generating are trivial, using a parser generator:

(defgrammar multiway-tree-string
    :terminals ((label   "[^^]")) ; one char, not ^
    :start tree
    :rules ((--> tree
                 (opt node)
                 :action (if (null $1)
                             (make-empty-multiway-tree)
                             $1)) ; it's identity, but make-empty-multiway-tree
                                        ; could be defined otherwise.
            (--> node
                 label (rep node) "^"
                 :action (make-multiway-tree :label (read-from-string (second $1))
                                             :children $2))))

;; (PARSE-MULTIWAY-TREE-STRING "afg^^c^bd^e^^^")
;; --> #S(MULTIWAY-TREE
;;        :LABEL A
;;        :CHILDREN (#S(MULTIWAY-TREE
;;                      :LABEL F
;;                      :CHILDREN (#S(MULTIWAY-TREE
;;                                    :LABEL G
;;                                    :CHILDREN NIL)))
;;                   #S(MULTIWAY-TREE
;;                      :LABEL C
;;                      :CHILDREN NIL)
;;                   #S(MULTIWAY-TREE
;;                      :LABEL B
;;                      :CHILDREN (#S(MULTIWAY-TREE
;;                                    :LABEL D
;;                                    :CHILDREN NIL)
;;                                 #S(MULTIWAY-TREE
;;                                    :LABEL E
;;                                    :CHILDREN NIL)))))
;;
;; (PARSE-MULTIWAY-TREE-STRING "")
;; --> NIL 


(defun multiway-tree-from-string (string)
  (PARSE-MULTIWAY-TREE-STRING string))


;; and walking the tree:

(defun multiway-tree-to-string (tree)
  (cond ((empty-multiway-tree-p tree) "")
        ((non-empty-multiway-tree-p tree)
         (format nil "~A~{~A~}^"
                 (multiway-tree-label tree)
                 (mapcar (function multiway-tree-to-string)
                         (multiway-tree-children tree))))
        (t (error "Not a multiway-tree ~S" tree))))


;; (multiway-tree-to-string #S(MULTIWAY-TREE :LABEL A :CHILDREN (#S(MULTIWAY-TREE :LABEL F :CHILDREN (#S(MULTIWAY-TREE :LABEL G :CHILDREN NIL))) #S(MULTIWAY-TREE :LABEL C :CHILDREN NIL) #S(MULTIWAY-TREE :LABEL B :CHILDREN (#S(MULTIWAY-TREE :LABEL D :CHILDREN NIL) #S(MULTIWAY-TREE :LABEL E :CHILDREN NIL))))))
;; --> "AFG^^C^BD^E^^^"

;;;; THE END ;;;;
