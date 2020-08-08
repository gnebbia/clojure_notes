#-(and) "
   
P73 (**) Lisp-like tree representation

    There is a particular notation for multiway trees in Lisp. Lisp is
    a prominent functional programming language, which is used
    primarily for artificial intelligence problems. As such it is one
    of the main competitors of Prolog. In Lisp almost everything is a
    list, just as in Prolog everything is a term.
   
    The following pictures show how multiway tree structures are
    represented in Lisp.

    Note that in the \"lispy\" notation a node with successors (children)
    in the tree is always the first element in a list, followed by its
    children. The \"lispy\" representation of a multiway tree is a
    sequence of atoms and parentheses ' (' and ')', which we shall
    collectively call \"tokens\". We can represent this sequence of
    tokens as a Prolog list; e.g. the lispy expression (a (b c)) could
    be represented as the Prolog list ['(', a, '(', b, c, ')',
    ')']. Write a predicate tree-ltl(T,LTL) which constructs the
    \"lispy token list\" LTL if the tree is given as term T in the usual
    Prolog notation.
   
    Example:
    * tree-ltl(t(a,[t(b,[]),t(c,[])]),LTL).
    LTL = ['(', a, '(', b, c, ')', ')']
   
    As a second, even more interesting exercise try to rewrite
    tree-ltl/2 in a way that the inverse conversion is also possible:
    Given the list LTL, construct the Prolog tree T. Use difference
    lists.
   
"
(load "rdp")
(use-package :com.informatimago.rdp)
(load "p70")

"
Again there are several problem with the problem statement.

In lisp, there are no parentheses.  Only cons cells and atoms.  The
lisp expression (a (b c)) doesn't represent a sequence of symbols and
parentheses, but a structure made of cons cells and symbols:

    +-----------------------------------+
    | (a (b c))                         |
    |                                   |
    | +---+---+   +---+---+             |
    | | * | * |-->| * |NIL|             |
    | +---+---+   +---+---+             |
    |   |           |                   |
    |   v           v                   |
    | +---+       +---+---+   +---+---+ |
    | | A |       | * | * |-->| * |NIL| |
    | +---+       +---+---+   +---+---+ |
    |               |           |       |
    |               v           v       |
    |             +---+       +---+     |
    |             | B |       | C |     |
    |             +---+       +---+     |
    +-----------------------------------+


The correct representation of such a structure in prolog would be:

   [a,[b,c]]

and not the proposed:

   ['(', a, '(', b, c, ')', ')']

A textual representation of that structure would be a STRING, not a
list of characters:

   \"(a (b c))\"

we can build a list of characters as an intermediate representation of
the string, but it is not too useful.  It would not be done usually in
lisp programs.

On the other hand, when writing a parser, it would be possible to
separate the lexer from the parser, having the lexer generate a list
of tokens to be passed to the parser.



Notice also that if the problem was to produce the multiway-tree as a
sexp where each node is represented by a list containing the label as
first element, and a sublist containing the children as second
element, then we would just have to give the (:type list) option to
the defstruct to have it represent the trees that way!  But the syntax
defined above specifies the irregularty that leaf nodes are
represented by the mere label of the leaf, instead of a list with the
label and an empty list of children.

"

;; Badass solution:

(defstruct (multiway-tree
             (:type list))
  label
  children)

;; (parse-multiway-tree-string "AFG^^C^BD^E^^^")
;; --> (A ((F ((G NIL))) (C NIL) (B ((D NIL) (E NIL)))))



;; Let's generate the lisp sexp with the leaves reprensted by their
;; labels.  This doesn't need that the multiway trees be represented a
;; lists, since we keep using the functional abstraction.

(defun process-leaves (tree)
  (cond
    ((empty-multiway-tree-p tree) tree)
    ((endp (multiway-tree-children tree)) (multiway-tree-label tree))
    (t (cons (multiway-tree-label tree)
             (mapcar (function process-leaves)
                     (multiway-tree-children tree))))))

(assert (equal (process-leaves (parse-multiway-tree-string "AFG^^C^BD^E^^^"))
               '(A (F G) C (B D E))))

;; Badass solution, using lisp sexps to generate first the string,
;; then the wanted list:


(defun my-prin1-to-string (object)
  (let ((*print-circle*   nil)
        (*print-case*     :upcase)
        (*print-readably* nil)
        (*print-pretty*   nil)
        (*print-base*     10.)
        (*print-radix*    nil)
        (*print-level*    nil)
        (*print-length*   nil)
        (*print-lines*    nil))
    (prin1-to-string object)))

(defun tree-ltl (tree)
  ;; How unfunny is that!
  (coerce (remove #\space (my-prin1-to-string (process-leaves tree))) 'list))


(assert (equal (tree-ltl (parse-multiway-tree-string "AFG^^C^BD^E^^^"))
               '(#\( #\A #\( #\F #\G #\) #\C #\( #\B #\D #\E #\) #\))))


;; We could also make the non-parenthesis characters back into symbols:

(defun tree-ltl (tree)
  (map 'list
       (lambda (ch)
         (if (alphanumericp ch)
             (intern (string ch))
             ch))
       (remove #\space  (my-prin1-to-string (process-leaves tree)))))

(assert (equal (tree-ltl (parse-multiway-tree-string "AFG^^C^BD^E^^^"))
               '(#\( A #\( F G #\) C #\( B D E #\) #\))))


;; Finally we could also repeat again and again the same tree walking
;; and generation of the list:


(defun tree-ltl (tree)
  (cond
    ((empty-multiway-tree-p tree) "") ; should occur only when root is empty.
    ((endp (multiway-tree-children tree))
     (list (multiway-tree-label tree)))
    (t (nconc
        (list #\( (multiway-tree-label tree))
        (mapcan (function tree-ltl)
                (multiway-tree-children tree))
        (list #\))))))


(assert (equal (tree-ltl (parse-multiway-tree-string "AFG^^C^BD^E^^^"))
               '(#\( A #\( F G #\) C #\( B D E #\) #\))))


;; Now, for the inverse function, parsing the list could be done
;; directly; we'd have to provide a pseudo-lexer to replace the lexer
;; that scans source strings.   For a simple solution we will just
;; convert the list into a string and let the generated scanner do its
;; work:


(defgrammar multiway-tree-parenthesized-string
    :terminals ((label   "[^^]")) ; one char, not ^
    :start tree
    :rules ((--> tree
                 (opt (alt par-node leaf))
                 :action (if (null $1)
                             (make-empty-multiway-tree)
                             $1))
                                        ; it's identity, but make-empty-multiway-tree
                                        ; could be defined otherwise.
            (--> par-node
                 "(" label (rep tree) ")"
                 :action (make-multiway-tree :label (read-from-string (second $2))
                                             :children $3))
            (--> leaf
                 label
                 :action (make-multiway-tree :label (read-from-string (second $1))))))

(defun ltl-tree (ltl)
  (parse-multiway-tree-parenthesized-string
   (format nil "~{~A~}" ltl)))

(assert (equal (ltl-tree '(#\( A #\( F G #\) C #\( B D E #\) #\)))
               (parse-multiway-tree-string "AFG^^C^BD^E^^^")))




;; Another solution would be to use the lisp reader, since the list
;; should contain well balanced parentheses, and then convert the
;; obtained sexp into a tree.  Notice how much simplier this is, to
;; process simple grammar sufficiently similar to sexps: there's no
;; need to involve the complexities of a parser generator.

(defun multiway-tree-from-sexp (node)
  (cond
    ((null node) (make-empty-multiway-tree))
    ((atom node) (make-multiway-tree :label node)) ; a leaf
    (t (destructuring-bind (label &rest children) node
         (make-multiway-tree :label label
                             :children (mapcar (function multiway-tree-from-sexp)
                                               children))))))

(defun ltl-tree (ltl)
  (multiway-tree-from-sexp (let ((*read-eval* nil)
                                 (*read-base* 10.)
                                 (*readtable* (copy-readtable nil)))
                             (read-from-string (format nil "~{ ~A ~}" ltl)))
                           ;; we get a list such as (A (F G) C (B D E))
                           ))

(assert (equal (ltl-tree '(#\( A #\( F G #\) C #\( B D E #\) #\)))
               (parse-multiway-tree-string "AFG^^C^BD^E^^^")))


;;;; THE END ;;;;


