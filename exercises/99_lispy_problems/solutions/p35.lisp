#-(and) "

P35 (**) Determine the prime factors of a given positive integer.
    Construct a flat list containing the prime factors in ascending order.
    Example:
    * (prime-factors 315)
    (3 3 5 7)
"


;; Solution using a library function...

(defun compute-primes-to (n)
  "
DO:     Compute an Eratostene sieve to find all prime numbers up to N.
RETURN: A sorted array of all prime numbers up to n.
"
  (cond
    ((< n 2) #())
    ((= n 2) #(2))
    ((= n 3) #(2 3))
    (t
     (let (bits-max bits bit (prime-count 2) (cur-prime 3) primes)
       ;; (SETF N (+ N (IF (ODDP N) 3 2)))
       (setf n (- n (if (oddp n) 3 2))) ; base of bits array is 3.
       (setf bits-max (/ n 2))
       ;; set the bitset to full bits;
       (setf bits (make-array (list bits-max) :initial-element 1 :element-type 'bit))
       (loop :while (< cur-prime n) :do
         (setf bit (+ cur-prime (/ (- cur-prime 3) 2)))
         (loop :while (< bit bits-max) :do
           (setf (aref bits bit) 0)
           (incf bit cur-prime))
         (setf bit (1+ (/ (- cur-prime 3) 2)))
         ;; search next prime
         (setf bit (position 1 bits :start bit))
         (if bit
             (setf cur-prime (+ bit bit 3)
                   prime-count (1+ prime-count))
             (setf cur-prime n)))
       ;; gather the primes.
       (setf primes (make-array (list prime-count) :element-type 'integer))
       (let ((curnum 0))
         (setf (aref primes curnum) 2)
         (incf curnum)
         (setf (aref primes curnum) 3)
         (incf curnum)
         (setf cur-prime 3)
         (setf bit 0)
         (setf bit (position 1 bits :start (1+ bit)))
         (loop :while bit :do
           (setf cur-prime (+ bit bit 3))
           (setf (aref primes curnum) cur-prime)
           (incf curnum)
           (setf bit (position 1 bits :start (1+ bit)))))
       primes))))


(defun factorize (n &optional (primes nil))
  "
N:        an INTEGER
PRIMES:   a VECTOR of prime factors sorted in increasing order.
RETURN:   a SEXP of the form: (* uncomensurate-factor
                                 [ prime | (EXPT prime exponent) ]... [ -1 ] )
"
  (let ((primes (or primes (compute-primes-to (1+ (isqrt n)))))
        (factors '())
        (prime-idx 0) )
    (unless (integerp n)
      (error "I can only decompose integer values."))
    (when (< n 0)
      (push -1 factors)
      (setf n (- n)))
    (loop :while (and (< prime-idx (length primes)) (< 1 n)) :do
       (let ((prime (elt primes prime-idx))
             (expo 0))
         (multiple-value-bind (q r) (truncate n prime)
           (loop :while (zerop r) :do
              (incf expo)
              (setf n q)
              (multiple-value-setq (q r) (truncate n prime))))
         (when (plusp expo)
           (push (if (= 1 expo)
                     prime
                     (list 'expt prime expo))
                 factors)))
       (incf prime-idx))
    (when (< 1 n)
      (push n factors))
    (cons '* factors)))


(defun prime-factors (n)
  (mapcan (lambda (factor)
            (cond
              ((and (listp factor) (eql 'expt (first factor)))
               (destructuring-bind (expt prime expo) factor
                 (declare (ignore expt))
                 (make-list expo :initial-element prime)))
              (t (list factor))))
          (nreverse (cdr (factorize n)))))


;; (prime-factors 315)
;; --> (3 3 5 7)


;;;; THE END ;;;;
