#-(and) "
   
P40 (**) Goldbach's conjecture.

    Goldbach's conjecture says that every positive even number greater
    than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It
    is one of the most famous facts in number theory that has not been
    proved to be correct in the general case. It has been numerically
    confirmed up to very large numbers (much larger than we can go
    with our Prolog system). Write a predicate to find the two prime
    numbers that sum up to a given even integer.
   
    Example:
    * (goldbach 28)
    (5 23)
"

;;

(defun goldbach (n)
  (check-type n (integer 4))
  (assert (evenp n))
  (let ((primes (compute-primes-to n))
        (bits   (make-array (1+ n) :element-type 'bit :initial-element 0)))
    (loop
       :for p :across primes :do (setf (aref bits p) 1))
    (loop
       :for p :across primes
       :when (plusp (aref bits (- n p)))
       :do   (return (list p (- n p)))
       :finally (return '()))))


;; (loop :for i :from 4 :to 100 :by 2 :collect (goldbach i))
;; ((2 2) (3 3) (3 5) (3 7) (5 7) (3 11) (3 13) (5 13) (3 17) (3 19)
;; (5 19) (3 23) (5 23) (7 23) (3 29) (3 31) (5 31) (7 31) (3 37) (5
;; 37) (3 41) (3 43) (5 43) (3 47) (5 47) (7 47) (3 53) (5 53) (7 53)
;; (3 59) (3 61) (5 61) (7 61) (3 67) (5 67) (3 71) (3 73) (5 73) (7
;; 73) (3 79) (5 79) (3 83) (5 83) (7 83) (3 89) (5 89) (7 89) (19 79)
;; (3 97))

;;;; THE END ;;;;
