#-(and) "   
P32 (**) Determine the greatest common divisor of two positive integer numbers.
    Use Euclid's algorithm.
    Example:
    * (gcd 36 63)
    9
"


;; Smartass solution using Common Lisp:

(defun my-gcd (p q)
  (gcd p q))


;; Euclide's algorithm:

(defun my-gcd (p q)
  (cond
    ((= p q) p)
    ((< p q) (my-gcd p (- q p)))
    (t       (my-gcd q (- p q)))))


;; (loop
;;    :for p :from 1 :below 100
;;    :do (loop :for q :from 1 :below 100
;;           :do (assert (= (my-gcd p q) (gcd p q)) (p q)))
;;    :finally (return :success))
;; --> :SUCCESS

