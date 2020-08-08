#-(and) "
   
P37 (**) Calculate Euler's totient function phi(m) (improved).

    See problem P34 for the definition of Euler's totient function. If
    the list of the prime factors of a number m is known in the form
    of problem P36 then the function phi(m) can be efficiently
    calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the
    list of prime factors (and their multiplicities) of a given number
    m. Then phi(m) can be calculated with the following formula:
   
    phi(m) = (p1 - 1) * p1 ** (m1 - 1) + (p2 - 1) * p2 ** (m2 - 1) + (p3 - 1) * p3 ** (m3 - 1) + ...
   
    Note that a ** b stands for the b'th power of a.
"

;;; https://secure.wikimedia.org/wikipedia/en/wiki/Euler%27s_totient_function#Computing_Euler.27s_function

(defun phi (m)
  ;;   (p1 - 1) * p1 ** (m1 - 1)
  ;; + (p2 - 1) * p2 ** (m2 - 1)
  ;; + (p3 - 1) * p3 ** (m3 - 1)
  ;; + ...
  (reduce (function *)
          (mapcar (lambda (item)
                    (destructuring-bind (p-i m-i) item
                      (* (1- p-i) (expt p-i (1- m-i)))))
                  (prime-factors-mult m))))


;; There's something wrong, phi is not equal to totient-phi, so there
;; must be some error in the problem statements.  We need to check them.

;; (loop
;;    :for n :from 2 :to 100
;;    :do (unless (= (totient-phi n) (phi n))
;;          (format t "(totient-phi ~A) = ~A /= ~A = (phi ~A)~%"
;;                   n (totient-phi n) (phi n) n)))


;;;; THE END ;;;;
