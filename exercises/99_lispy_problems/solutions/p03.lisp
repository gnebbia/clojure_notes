#-(and)"

P03 (*) Find the K'th element of a list.
    The first element in the list is number 1.
    Example:
    * (element-at '(a b c d e) 3)
    C

"

;; The nice, recursive solution:

(defun element-at (list index)
  (cond
    ((not (integerp index)) (error "Non integer index ~A" index))
    ((not (plusp index))    (error "Non strictly positive index ~A" index))
    ((endp list)            (error "List too short."))
    ((= 1 index)            (first list))
    (t                      (element-at (rest list) (1- index)))))


;; The efficient, iterative solution:

(defun element-at (list index)
  (cond
    ((not (integerp index)) (error "Non integer index ~A" index))
    ((not (plusp index))    (error "Non strictly positive index ~A" index))
    (t
     (loop
        :for result :on list
        :while (plusp (decf index))
        :finally (if (endp result)
                     (error "List too short.")
                     (return (first result)))))))


;; The smartass, Common Lisp solution:

(defun element-at (list index)
  (cond
    ((not (integerp index)) (error "Non integer index ~A" index))
    ((not (plusp index))    (error "Non strictly positive index ~A" index))
    (t                      (elt list (1- index)))))

;;;; THE END ;;;;
