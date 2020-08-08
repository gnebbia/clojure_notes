#-(and) "
P18 (**) Extract a slice from a list.
    Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of
    the original list (both limits included). Start counting the elements with 1.
   
    Example:
    * (slice '(a b c d e f g h i k) 3 7)
    (C D E F G)
"


;; Recursive solution:

(defun slice (list start last)
  (cond ((not (<= 1 start last)) (error "Invalid indices."))
        ((= 1 start) (first (split list last)))
        (t           (slice (rest list) (1- start) (1- last)))))

;; Iterative solution:

(defun slice (list start last)
  (loop
     :repeat (1- start)
     :for rest :on list
     :finally (return (loop
                         :repeat (- last start -1)
                         :for item :in rest
                         :collect item))))


;; Functional solution, using Common Lisp:

(defun slice (list start last)
  (let ((rest  (nthcdr (1- start) list)))
    (ldiff rest (nthcdr (- last start -1) rest))))


;; Smartass solution, using Common Lisp:

(defun slice (list start last)
  (subseq list (1- start) last))


;;;; THE END ;;;;
