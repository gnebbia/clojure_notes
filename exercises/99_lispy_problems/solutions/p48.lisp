#-(and) "

P48 (**) Truth tables for logical expressions (3).

    Generalize problem P47 in such a way that the logical expression
    may contain any number of logical variables. Define table/2 in a
    way that table(List,Expr) prints the truth table for the
    expression Expr, which contains the logical variables enumerated
    in List.
   
    Example:
    * table([A,B,C], A and (B or C) equ A and B or A and C).
    true true true true
    true true fail true
    true fail true true
    true fail fail true
    fail true true true
    fail true fail true
    fail fail true true
    fail fail fail true
"


(defun table (variables expr)
  (loop
     :with width = (length variables)
     :with size  = (expt 2 width)
     :for i :from (1- size) :downto 0  ; to display from true to fail.
     :do (let* ((bindings (loop
                             :for bit :from (1- width) :by -1
                             :for var :in variables
                             :collect (cons var
                                            (if (logbitp bit i)
                                                'true
                                                'fail))))                
                (v (evaluate-boolean expr bindings)))
           (format t "~{~:[fail~;true~] ~} ~:[fail~;true~]~%"
                   (mapcar (lambda (binding) (EQL 'TRUE (CDR BINDING))) BINDINGS)
                   V))))

;; (table '(a b c) (remove-identity (parse-logical-expression "(a and (b or c) equ a and b or a and c)")))
;; true true true  true
;; true true fail  true
;; true fail true  true
;; true fail fail  true
;; fail true true  true
;; fail true fail  true
;; fail fail true  true
;; fail fail fail  true
;; --> NIL

;;;; THE END ;;;;
