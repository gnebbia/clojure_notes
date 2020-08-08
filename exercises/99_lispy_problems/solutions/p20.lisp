#-(and) "

P20 (*) Remove the K'th element from a list.
    Example:
    * (remove-at '(a b c d) 2)
    (A C D)
"


;; Recursive solution, sharing the tail:

(defun remove-at (list index)
  (cond
    ((< index 1) (error "Invalid index"))
    ((= index 1) (rest list))
    (t           (cons (first list) (remove-at (rest list) (1- index))))))


;; Iterative solution, sharing the tail:

(defun remove-at (list index)
  (cond
    ((< index 1) (error "Invalid index"))
    (t           (loop
                    :for rest :on list
                    :repeat (1- index)
                    :collect (first rest) :into left
                    :finally (return (nconc left (rest rest)))))))

;; Iterative solution, copying the whole result:

(defun remove-at (list index)
  (cond
    ((< index 1) (error "Invalid index"))
    (t           (loop
                    :for item :in list
                    :for i :from 1
                    :unless (= i index) :collect item))))


;; Functional solution, sharing the tail:

(defun remove-at (list index)
  (append (subseq list 0 (1- index)) (nthcdr index list)))


;; Functional solution, copying the whole result:

(defun remove-at (list index)
  (append (subseq list 0 (1- index)) (copy-list (nthcdr index list))))

;; For both these functional solutions, we could replace append by
;; nconc, since subseq returns a new list, so we could just modify the
;; last cdr to attach it to the rest.



;; Smartass solution, using Common Lisp:

(defun remove-at (list index)
  (remove-if (constantly t) list :start (1- index) :end index))


;;;; THE END ;;;;
