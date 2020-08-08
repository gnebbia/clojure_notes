#-(and)"

P01 (*) Find the last box of a list.
    Example:
    * (my-last '(a b c d))
    (D)

"

;; The nice, recursive solution:

(defun my-last (list)
  (cond
    ((endp list)                (error "Empty list"))
    ((endp (rest list))         list)
    (t                          (my-last (rest list)))))

;; The efficient, iterative solution:

(defun my-last (list)
  (cond
    ((endp list)                (error "Empty list"))
    (t  (loop
           :for result :on list
           :until (endp (rest result))
           :finally (return result)))))


;; The smartass, Common Lisp solution:

(defun my-last (list)
  (last list))


;;;; THE END ;;;;
