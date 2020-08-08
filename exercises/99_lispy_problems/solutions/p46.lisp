#-(and) "
   
Logic and Codes

P46 (**) Truth tables for logical expressions.

    Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and
    equ/2 (for logical equivalence) which succeed or fail according to
    the result of their respective operations; e.g. and(A,B) will
    succeed, if and only if both A and B succeed. Note that A and B
    can be Prolog goals (not only the constants true and fail).
   
    A logical expression in two variables can then be written in
    prefix notation, as in the following example:
    and(or(A,B),nand(A,B)).
   
    Now, write a predicate table/3 which prints the truth table of a
    given logical expression in two variables.
   
    Example:
    * table(A,B,and(A,or(A,B))).
    true true true
    true fail true
    fail true fail
    fail fail fail
"



;; The first question is somewhat meaningless in Lisp, since we have
;; values, not goals, with applicative evaluation, where the
;; subexpressions are evaluated before calling a function.  

(defun .and.  (a b) (cond (a b) (t nil)))
(defun .or.   (a b) (cond (a) (b) (t nil)))
(defun .nand. (a b) (not (.and. a b)))
(defun .nor.  (a b) (not (.or.  a b)))
(defun .xor.  (a b) (cond (a (not b)) (b t) (t nil)))
(defun .impl. (a b) (.or. (not a) b))
(defun .equ.  (a b) (cond (a b) (t (not b))))


;; On the other hand, AND and OR are defined as macro in lisp, to
;; implement short-cut evaluation.  Notice that in the case of xor and
;; equ, it doesn't matter, we need to evaluate both in any case.

(defmacro =and=  (a b) `(cond (,a ,b) (t nil)))
(defmacro =or=   (a b) `(cond (,a) (,b) (t nil)))
(defmacro =nand= (a b) `(=or=  (not ,a) (not ,b)))
(defmacro =nor=  (a b) `(=and= (not ,a) (not ,b)))
(defmacro =xor=  (a b) `(cond (,a (not ,b)) (,b t) (t nil)))
(defmacro =impl= (a b) `(=or= (not ,a) ,b))
(defmacro =equ=  (a b) `(cond (,a ,b) (t (not ,b))))



;; For the second question, we can write a function taking two
;; variable names (two symbols) and a symbolic expression (sexp), and
;; print a truth table.  Here is a solution using EVAL, which is not
;; safe, since expr could be bound to any lisp form, even something
;; "dangerous" such as a delete-file form.

(defun table (a b expr)
  (loop :for p :in '(t nil) :do
     (loop :for q :in '(t nil) :do
        (let ((v (eval `(let ((,a ,p) (,b ,q)) ,expr))))
          (format t "~:[fail~;true~] ~:[fail~;true~] ~:[fail~;true~]~%" p q v)))))



;; Here is a solution where we implement our own evaluation function:

(defmacro nand (a b) `(or  (not ,a) (not ,b)))
(defmacro nor  (a b) `(and (not ,a) (not ,b)))
(defun    xor  (a b) (cond (a (not b)) (b t) (t nil)))
(defmacro impl (a b) `(or (not ,a) ,b))
(defun    equ  (a b) (cond (a b) (t (not b))))

(defun evaluate-boolean (expression bindings)
  "Evaluates the boolean expression.  Returns t or NIL.

expression := variable | constant | '(' operator expression expression ')' | '(' 'not' expression ')' .
constant   := 'true' | 'fail' .
variable   := symbol .
operator   := 'and' | 'or' | 'nand' | 'nor' | 'xor' | 'impl' | 'equ' .

bindings   is a list of pairs (variable . constant).
"
  (cond
    ((eq expression 'true) t)
    ((eq expression 'fail) nil)
    ((symbolp expression) (let ((pair (assoc expression bindings)))
                            (if pair
                                (progn
                                  (assert (member (cdr pair) '(true fail)))
                                  (eql 'true (cdr pair)))
                                (error "No variable named ~A in the bindings." expression))))
    ((atom expression) (error "Invalid atom ~A in the expression." expression))
    (t (case (length expression)
         ((2) (destructuring-bind (op subexpression) expression
                (case op
                  ((not) (not (evaluate-boolean subexpression bindings)))
                  (otherwise (error "Invalid operator ~A" op)))))
         ((3) (destructuring-bind (op left right) expression
                (case op
                  ((and)  (and  (evaluate-boolean left bindings) (evaluate-boolean right bindings)))
                  ((or)   (or   (evaluate-boolean left bindings) (evaluate-boolean right bindings)))
                  ((nand) (nand (evaluate-boolean left bindings) (evaluate-boolean right bindings)))
                  ((nor)  (nor  (evaluate-boolean left bindings) (evaluate-boolean right bindings)))
                  ((xor)  (xor  (evaluate-boolean left bindings) (evaluate-boolean right bindings)))
                  ((impl) (impl (evaluate-boolean left bindings) (evaluate-boolean right bindings)))
                  ((equ)  (equ  (evaluate-boolean left bindings) (evaluate-boolean right bindings)))
                  (otherwise (error "Invalid operator ~A" op)))))
         (otherwise (error "Invalid expression ~A" expression))))))


(defun table (a b expr)
  (loop :for p :in '(true fail) :do
     (loop :for q :in '(true fail) :do
        (let ((v (evaluate-boolean expr (list (cons a p) (cons b q)))))
          (format t "~:[fail~;true~] ~:[fail~;true~] ~:[fail~;true~]~%"
                  (eql 'true p) (eql 'true q) v)))))


;; (table 'a 'b '(and a (or a b)))
;; true true true
;; true fail true
;; fail true fail
;; fail fail fail
;; NIL

;;;; THE END ;;;;
