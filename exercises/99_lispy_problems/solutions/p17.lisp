#-(and) "

P17 (*) Split a list into two parts; the length of the first part is given.
    Do not use any predefined predicates.
   
    Example:
    * (split '(a b c d e f g h i k) 3)
    ( (A B C) (D E F G H I K))
"



;; Recursive function:

(defun split (list count)
  (if (plusp count)
      (destructuring-bind (left right) (split (rest list) (1- count))
        (list (cons (first list) left) right))
      (list '() list)))


;; Iterative function:

(defun split (list count)
  (loop
     :for rest :on list
     :repeat count
     :collect (first rest) :into left
     :finally (return (list left rest))))


;; Smartass solution, using Common Lisp.

(defun split (list count)
  (list (subseq list 0 count)
        (subseq list count)))


;; Smartass solution, using Common Lisp, and sharing the tail:

(defun split (list count)
  (list (subseq list 0 count)
        (nthcdr count list)))


;;;; THE END ;;;;
