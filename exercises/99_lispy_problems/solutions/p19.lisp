#-(and) "

P19 (**) Rotate a list N places to the left.
    Examples:
    * (rotate '(a b c d e f g h) 3)
    (D E F G H A B C)
   
    * (rotate '(a b c d e f g h) -2)
    (G H A B C D E F)
   
    Hint: Use the predefined functions length and append, as well as the result of problem P17.
"

;; Solution using the function of p17:

(defun rotate (list count)
  (if (minusp count)
      (rotate list (+ (length list) count))
      (destructuring-bind (left right) (split list count)
        ;; Notice: depending on the implementation of split, right
        ;; might share structure with list, therefore we cannot use
        ;; nconc indiscriminately on it!
        (append right left))))


;; Smartass solution, using Common Lisp:

(defun rotate (list count)
  (if (minusp count)
      (rotate list (+ (length list) count))
      (nconc (subseq list count) (subseq list 0 count))))



;;;; THE END ;;;;
