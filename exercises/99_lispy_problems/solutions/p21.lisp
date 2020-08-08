#-(and) "

P21 (*) Insert an element at a given position into a list.
    Example:
    * (insert-at 'alfa '(a b c d) 2)
    (A ALFA B C D)
"


;; Recursive solution, sharing the tail:

(defun insert-at (item list index)
  (cond
    ((< index 1) (error "Index too small ~A" index))
    ((= index 1) (cons item list))
    ((endp list) (error "Index too big"))
    (t (cons (first list) (insert-at item (rest list) (1- index))))))



;; Functional solution, using the split function of p17.

(defun insert-at (item list index)
  (destructuring-bind (left right) (split list (1- index))
    (append left (list item) right)))



;; Smartass solution, using Common Lisp, sharing the tail:

(defun insert-at (item list index)
  (append (subseq list 0 (1- index))
          (list item)
          (nthcdr (1- index) list)))


;; Smartass solution, using Common Lisp, copying the whole result:

(defun insert-at (item list index)
  (concatenate 'list
    (subseq list 0 (1- index))
    (list item)
    (nthcdr (1- index) list)))


;; Smartass solution, using Common Lisp, modifying the original list!

(defun insert-at (item list index)
  (cond
    ((< index 1) (error "Index too small ~A" index))
    ((= index 1) (cons item list))
    (t (push item (cdr (nthcdr (- index 2) list)))
       list)))

;; (let ((list (list 'a 'b 'c 'd)))
;;   (values list
;;           (insert-at 'alfa list 2)))
;; --> (A ALFA B C D)
;;     (A ALFA B C D)
;;
;; (let ((list (list 'a 'b 'c 'd)))
;;   (values list
;;           (insert-at 'alfa list 1)))
;; --> (A B C D)
;;     (ALFA A B C D)

;;;; THE END ;;;;
