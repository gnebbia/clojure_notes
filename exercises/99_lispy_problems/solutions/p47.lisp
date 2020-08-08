#-(and) "
   
P47 (*) Truth tables for logical expressions (2).

    Continue problem P46 by defining and/2, or/2, etc as being
    operators. This allows to write the logical expression in the more
    natural way, as in the example: A and (A or not B). Define
    operator precedence as usual; i.e. as in Java.
   
    Example:
    * table(A,B, A and (A or not B)).
    true true true
    true fail true
    fail true fail
    fail fail fail
"

;; Again, this question doesn't make much sense in lisp.
;;
;; To have fun, we could interpret it as requesting parsing a list in
;; infix notation and translating it to prefix notation.
;;
;; (infix-to-prefix '(a and (a or not b))) --> (and a (or a (not b)))
;;
;; but notice that we need a list anyways, and that parenthesized
;; subexpressions are actually sublists, there's no parentheses to be
;; parsed.
;;
;; On the other hand, we could write a full lexer and parser for
;; prolog syntax, but that would be out of scope for this exercise.
;;
;;
;; Java operator precedences are:
;;
;; Priority     Operators       Operation                       Associativity
;; 1            [ ]             array index                            left
;;              ()              method call
;;              .               member access
;; 2            ++              pre- or postfix increment              right
;;              --              pre- or postfix decrement
;;              + -             unary plus, minus
;;              ~               bitwise NOT
;;              !               boolean (logical) NOT
;;              (type)          type cast
;;              new             object creation
;; 3            * / %           multiplication, division, remainder    left
;; 4            + -             addition, substraction                 left
;;              +               string concatenation
;; 5            <<              signed bit shift left                  left
;;              >>              signed bit shift right
;;              >>>             unsigned bit shift right
;; 6            < <=            less than, less than or equal to       left
;;              > >=            greater than, greater than or equal to
;;              instanceof      reference test
;; 7            ==              equal to                               left
;;              !=              not equal to
;; 8            &               bitwise AND                            left
;;              &               boolean (logical) AND
;; 9            ^               bitwise XOR                            left
;;              ^               boolean (logical) XOR
;; 10           |               bitwise OR                             left
;;              |               boolean (logical) OR
;; 11           &&              boolean (logical) AND                  left
;; 12           ||              boolean (logical) OR                   left
;; 13           ? :             conditional                            right
;; 14           =               assignment                             right
;;              *= /= += -= %=
;;              <<= >>= >>>=
;;              &= ^= |=        combinated assignment
;;                              (operation and assignment)
;;
;; There are no NAND, NOR, EQU, or IMPL, but there are several AND and
;; OR, with different precedences!  What a fucking problem statement!
;;
;; So we will write a parser that is parameterized by the precedences,
;; and we'll see later what is needed.

;; For this, we use a simple recursive-descend parser generator:

(load "rdp.lisp")
(use-package :com.informatimago.rdp)



;; Prefix or suffix operators must have arity = 1, and therefore
;; don't have any specific associativity dirrection other than their
;; being prefix or suffix.
;; x not not not  not/1 suffix (((x not) not) not)
;; not not not x  not/1 prefix (not (not (not x)))


;; Infix operators must have an arity > 1 (usually 2), and must have
;; either left or right associativity.
;;
;;  a op b op c   op/2 left    (a op b) op b
;;  a op b op c   op/2 right   a op (b op c)



;; We define an operator level as a list containing the precedence
;; level (smaller number, higher precendence), the arity, the
;; position-and-associativity (:prefix, :suffix, :infix-left or
;; :infix-right), and the list of operators:


(defstruct (level (:type list))
  (precedence    0       :type integer)
  (arity         0       :type integer)
  (position      :prefix :type (member :prefix :suffix :infix-left :infix-right))
  (operators     '()     :type list))



(defun leftify (operators expr)
  "
Transforms a right-associative operation tree EXPR of OPERATORS, into
a leflt-associative one.   (op a (op b c)) --> (op (op a b) c)
"
  (labels ((flatten (expr flattened)
             (cond
               ((atom expr) (cons expr flattened))
               ((member (first expr) operators)
                (flatten (third expr) (list* (first expr) (second expr) flattened)))
               (t (cons expr flattened))))
           (unflatten-left (expr flattened)
             (if (endp flattened)
                 expr
                 (unflatten-left (list (first flattened) expr (second flattened))
                                 (rest (rest flattened))))))
    (let ((flattened (nreverse (flatten expr '()))))
      (unflatten-left (first flattened) (rest flattened)))))


(defun production-var (n)
  "Makes a production variable $n"
  (intern (format nil "$~A" n)))


(defun token-to-lisp (token)
  ;; rdp tokens are (terminal "text" position)
  ;; Since we take care of naming our operator terminals as lisp
  ;; operators, we can just extract them from the  tokens.
  ;; For variables, we cl:read the text.
  (if (atom token)
      token
      (case (first token)
        ((identifier) (read-from-string (second token)))
        ((true)       't)
        ((fail)       'nil)
        (otherwise    (first token)))))


(defun generate-operator-level-rules (non-terminal inferior-non-terminal level)
  "Generates a grammar rule to parse the given operator level, and
build the corresponding expression tree."
  (let ((op-non-terminal (intern (format nil "~A-OP" non-terminal))))
    (ecase (level-position level)

      ((:prefix)
       `((--> ,non-terminal
              (alt ,op-non-terminal ,inferior-non-terminal)
              :action $1)
         (--> ,op-non-terminal
              (alt ,@(level-operators level))
              ,@(make-list (level-arity level) :initial-element inferior-non-terminal)
              ;; (opt (seq (alt ,@(level-operators level))
              ;;           ,@(make-list (level-arity level) :initial-element inferior-non-terminal))
              ;;      ,inferior-non-terminal)
              :action (list (token-to-lisp $1)
                            ,@(loop
                                 :repeat (level-arity level)
                                 :for i :from 2 :collect (production-var i))))))

      ((:suffix)
       `((--> ,non-terminal
              (alt ,op-non-terminal ,inferior-non-terminal)
              :action $1)
         (--> ,op-non-terminal
              ,@(make-list (level-arity level) :initial-element inferior-non-terminal)
              (alt ,@(level-operators level))
              :action (list (token-to-lisp ,(production-var (1+ (level-arity level))))
                            ,@(loop
                                 :repeat  (level-arity level)
                                 :for i :from 1 :collect (production-var i))))))

      ;; We're using a recursive-descend parser, so we can have only
      ;; right-recursive rules.  Therefore we will just collect the list
      ;; of operations at the grammar level, and implement the
      ;; associativity in the action.
      ;;
      ;; (--> factor
      ;;      term op factor) ; :infix-right term op (term op term) 
      ;; 
      ;; (--> factor
      ;;      factor op term) ; :infix-left  (term op term) op term
      
      ((:infix-left :infix-right)
       (assert (= 2 (level-arity level))
               (level) "Infix operators with an arity different from 2 are not implemented.")
       `((--> ,non-terminal
              ,op-non-terminal
              :action ,(if (eql :infix-left (level-position level))
                           `(if (and (listp $1) (= 3 (length $1)))
                                (leftify ',(level-operators level) $1)
                                $1)
                           `$1))
         (--> ,op-non-terminal
              ,inferior-non-terminal (opt (alt ,@(level-operators level)) ,op-non-terminal)
              :action (if $2
                          (destructuring-bind (op right) $2
                            (list (token-to-lisp op) $1 right))
                          $1)))))))


(defun generate-operator-grammar (name operator-levels)
  "Generate a RDP grammar for the operators given in OPERATOR-LEVELS.
This will create a function named PARSE-{NAME}."
  (let* ((levels        (sort (copy-list operator-levels) (function >)
                              :key (function level-precedence)))
         (non-terminals (nconc (loop
                                  :for level :in levels
                                  :collect (intern (format nil "~{~A~^/~}-FACTOR" (level-operators level))))
                               (list 'term)))
         (terminals     (nconc (mapcan (lambda (level)
                                         (mapcar (lambda (operator) (list operator (string-downcase operator)))
                                                 (level-operators level)))
                                       levels)
                               '((true       "true")
                                 (fail       "fail")
                                 (identifier "[A-Za-z][-A-Za-z0-9]*"))))
         (rules         `((--> ,(first (last non-terminals))
                               (alt constant variable parenthesized-expression)
                               :action $1)
                          ;; We need to wrap terms in an identity operator to
                          ;; avoid lefitification of the first non-terminal:
                          ;; (a impl b) impl (c impl d) must stay that way.
                          (--> parenthesized-expression
                               "(" ,(first non-terminals) ")"
                               :action (list 'identity $2))
                          (--> constant (alt true fail)
                               :action (token-to-lisp $1))
                          (--> variable identifier
                               :action (token-to-lisp $1))
                          ,@(reduce (function append)
                                    (mapcar (function generate-operator-level-rules)
                                            non-terminals
                                            (rest non-terminals)
                                            levels)
                                    :from-end t))))
    #+debug
    (print `(com.informatimago.rdp:generate-grammar
             ,name
             :terminals ',terminals
             :start ',(first non-terminals)
             :rules ',rules))
    (com.informatimago.rdp:generate-grammar
     name
     :terminals terminals
     :start (first non-terminals)
     :rules rules)))





(defparameter *operators* '(( 2 1 :prefix      (not))
                            ( 8 2 :infix-left  (and nand))
                            ( 9 2 :infix-left  (xor equ))
                            (10 2 :infix-left  (or  nor))
                            (12 2 :infix-left  (impl))))


(generate-operator-grammar 'logical-expression *operators*)


(defun test/operator-grammar ()
  (loop
     :for (source expected)
     :in '(("a" a)
           ("true" t)
           ("fail" nil)
           ("a and b" (and a b))
           ("(a impl b) impl (c impl d)" (impl (identity (impl a b)) (identity (impl c d))))
           ("a and b and c and d" (AND (AND (AND a b) c) D))
           ("a and b and c or d and e and f or g and i and j"
            (or (or (and (and a b) c) (and (and d e) f))  (and (and g i) j)))
           ("(a xor b) equ (not a xor not b)"
            (equ (identity (xor a b)) (identity (xor (not a) (not b))))))
     :do (let ((result (handler-case (PARSE-LOGICAL-EXPRESSION source)
                         (error (err) (princ err) (terpri) :error))))
           (assert (equal result expected)
                   (source)
                   "Parsing the logical expression ~S~%               gave ~S ~%instead of expected ~S"
                   source result expected)))
  :success)


(defun remove-identity (expr)
  (cond
    ((atom expr) expr)
    ((eql 'identity (first expr)) (remove-identity (second expr)))
    (t (cons (first expr) (mapcar (function remove-identity) (rest expr))))))


;; (table 'a 'b (remove-identity (parse-logical-expression "a and (a or not b)")))
;; true true true
;; true fail true
;; fail true fail
;; fail fail fail
;; --> NIL



;;;; THE END ;;;;
