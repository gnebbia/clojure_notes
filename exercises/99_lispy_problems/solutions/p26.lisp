#-(and) "
   
P26 (**) Generate the combinations of K distinct objects chosen from
    the N elements of a list In how many ways can a committee of 3 be
    chosen from a group of 12 people? We all know that there are
    C(12,3) = 220 possibilities (C(N,K) denotes the well-known
    binomial coefficients). For pure mathematicians, this result may
    be great. But we want to really generate all the possibilities in
    a list.
   
    Example:
    * (combination 3 '(a b c d e f))
    ((A B C) (A B D) (A B E) ... )
"


;; A simple recursive solution:

(defun combinations (count list)
  (cond
    ((zerop count) '(())) ; one combination of zero element.
    ((endp list)   '())   ; no combination from noe element.
    (t (nconc (mapcar (let ((item (first list))) (lambda (combi) (cons item combi)))
                      (combinations (1- count) (rest list)))
              (combinations count (rest list))))))

;; (length (combinations 3  '(a b c d e f g h i j k l)))
;; --> 220

;;;; THE END ;;;;
