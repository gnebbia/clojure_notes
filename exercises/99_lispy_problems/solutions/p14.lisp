#-(and) "

P14 (*) Duplicate the elements of a list.
    Example:
    * (dupli '(a b c c d))
    (A A B B C C C C D D)
"


;; Nice functional solution:

(defun dupli (list)
  (mapcan (lambda (item) (list item item)) list))


;; Iterative solution:

(defun dupli (list)
  (loop
     :for item :in list
     :collect item
     :collect item))



;; Badass solution (using solution to p15):

(defun dupli (list)
  (repli list 2))


;;;; THE END ;;;;
