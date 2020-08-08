#-(and)"

P04 (*) Find the number of elements of a list.

"

;; The nice, recursive solution:

(defun number-of-elements (list)
  (cond
    ((endp list) 0)
    (t           (1+ (number-of-elements (rest list))))))


;; A tail-recursive solution (with accumulator):

(defun number-of-elements (list)
  (labels ((count-element (list count)
             (cond ((endp list) count)
                   (t           (count-element (rest list) (1+ count))))))
    (count-element list 0)))


;; The efficient, iterative solution:

(defun number-of-elements (list)
  (loop
     :for item :in list
     :count 1))


;; The smartass, Common Lisp solution:

(defun number-of-elements (list)
  (length list))


;;;; THE END ;;;;
