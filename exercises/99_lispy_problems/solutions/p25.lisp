#-(and) "

P25 (*) Generate a random permutation of the elements of a list.
    Example:
    * (rnd-permu '(a b c d e f))
    (B A D C E F)
   
    Hint: Use the solution of problem P23.
"


;; Assuming an random order solution for rnd-select:

(defun rnd-permu (list)
  (rnd-select list (length list)))


;; Generating the permutation from scratch:


(defun make-circular (list)
  (setf (cdr (last list)) list))


(defun rnd-permu (list)
  (when list
    (loop
       :with len = (length list)
       :with choices = (make-circular (copy-list list))
       :collect (pop (cdr (nthcdr (random len) choices)))
       :while (plusp (decf len)))))


;;;; THE END ;;;;
