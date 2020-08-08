#-(and) "
   
P49 (**) Gray code.

    An n-bit Gray code is a sequence of n-bit strings constructed
    according to certain rules. For example,

    n = 1: C(1) = ['0','1'].
    n = 2: C(2) = ['00','01','11','10'].
    n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
   
    Find out the construction rules and write a predicate with the
    following specification:
   
    % gray(N,C) :- C is the N-bit Gray code
   
    Can you apply the method of \"result caching\" in order to make the
    predicate more efficient, when it is to be used repeatedly?
"

;; In Lisp, we will instead implement a function (gray n) producing
;; the list of gray codes C(n).


;; Here is a solution giving the codes as strings of #\0 or #\1:

(defun gray (n)
  (if (= 1 n)
      (list "0" "1")
      (let ((gray-1 (gray (1- n))))
        (nconc (mapcar (lambda (code) (concatenate 'string "0" code))
                       gray-1)
               (mapcar (lambda (code) (concatenate 'string "1" code))
                       (nreverse gray-1))))))

;; Since we call (gray (1- n)) only once per call to (gray n), there's
;; no gain in memoizing gray, unless it is called repeatitively, in
;; which case, memoizing could help as caching any other function.


;; (gray 3) --> ("000" "001" "011" "010" "110" "111" "101" "100")



;; Here is a version giving the codes as integers:

(defun gray (n)
  (if (= 1 n)
      (list 0 1)
      (let ((gray-1 (gray (1- n))))
        (nconc gray-1
               (mapcar (lambda (code) (dpb 1 (byte 1 (1- n)) code))
                       (reverse gray-1))))))

;; (gray 3) --> (0 1 3 2 6 7 5 4)


;;;; THE END ;;;;
