#-(and) "   

P36 (**) Determine the prime factors of a given positive integer (2).
    Construct a list containing the prime factors and their multiplicity.
    Example:
    * (prime-factors-mult 315)
    ((3 2) (5 1) (7 1))
   
    Hint: The problem is similar to problem P13.
"

;; Solution, using the same library functions as in p35:

(defun prime-factors-mult (n)
  (mapcar (lambda (factor)
            (cond
              ((and (listp factor) (eql 'expt (first factor)))
               (destructuring-bind (expt prime expo) factor
                 (declare (ignore expt))
                 (list prime expo)))
              (t
               (list factor 1))))
          (nreverse (cdr (factorize n)))))


;; (prime-factors-mult 315)
;; --> ((3 2) (5 1) (7 1))


;;;; THE END ;;;;


