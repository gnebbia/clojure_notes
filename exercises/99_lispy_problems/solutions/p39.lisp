#-(and) "
   
P39 (*) A list of prime numbers.

    Given a range of integers by its lower and upper limit, construct
    a list of all prime numbers in that range.
"


;; Using library function from problem p35.
;; Note that to compute the primes greater than X, you need to know
;; the primes less or equal to X.

(defun primes-in-range (lower upper)
  (remove-if (lambda (x) (< x lower)) (compute-primes-to (1+ upper))))


;;;; THE END ;;;;
