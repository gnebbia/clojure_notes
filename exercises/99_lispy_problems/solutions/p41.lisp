#-(and) "

P41 (**) A list of Goldbach compositions.

    Given a range of integers by its lower and upper limit, print a
    list of all even numbers and their Goldbach composition.
   
    Example:
    * (goldbach-list 9 20)
    10 = 3 + 7
    12 = 5 + 7
    14 = 3 + 11
    16 = 3 + 13
    18 = 5 + 13
    20 = 3 + 17
   
    In most cases, if an even number is written as the sum of two
    prime numbers, one of them is very small. Very rarely, the primes
    are both bigger than say 50. Try to find out how many such cases
    there are in the range 2..3000.
   
    Example (for a print limit of 50):
    * (goldbach-list 1 2000 50)
    992 = 73 + 919
    1382 = 61 + 1321
    1856 = 67 + 1789
    1928 = 61 + 1867
"


(defun goldbach-list (lower upper &optional limit)
  (if limit
      (loop
         :for n :from (max 4 (* 2 (ceiling lower 2))) :to (* 2 (truncate upper 2)) :by 2
         :do (destructuring-bind (p q) (goldbach n)
               (when (and (<= limit p) (<= limit q))
                 (format t "~A = ~A + ~A~%" n p q))))
      (loop
         :for n :from (max 4 (* 2 (ceiling lower 2))) :to (* 2 (truncate upper 2)) :by 2
         :do (destructuring-bind (p q) (goldbach n)
               (format t "~A = ~A + ~A~%" n p q)))))

;; (goldbach-list 1 2000 50)
;; 
;; 992 = 73 + 919
;; 1382 = 61 + 1321
;; 1856 = 67 + 1789
;; 1928 = 61 + 1867
;; --> NIL



;;;; THE END ;;;;
