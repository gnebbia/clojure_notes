#-(and)"

P05 (*) Reverse a list.

"

;; The naive recursive solution, O(n²):

(defun naive-recursive-reverse (list)
  (if (endp list)
      '()
      (append (naive-recursive-reverse (rest list))
              (cons (first list) '()))))


;; A tail-recursive solution (with accumulator):

(defun recursive-reverse (list)
  (labels ((rev (list acc)
     (if (endp list)
          acc
          (rev (rest list) (cons (first list) acc)))))
      (rev list '())))


;; An iterative solution:

(defun iterative-reverse-do (list)
  (let ((acc '()))
    (do ((current list (rest current)))
        ((endp current) acc)
      (setf acc (cons (first current) acc)))))


(defun iterative-reverse-loop (list)
  (loop
     :with result = '()
     :for item :in list
     :do (push item result)
     :finally (return result)))


;; The smartass, Common Lisp solution:

(defun my-reverse (list)
  (reverse list))



;; Other Common Lisp solutions:

(defun reduce-reverse-from-start (list)
  (reduce (lambda (d a) (cons a d)) list :initial-value '()))

(defun reduce-reverse-from-end (list)
  (reduce (lambda (a d) (append d (list a))) list :initial-value '()  :from-end t))

(defun reduce-reverse-from-end-with-pointer (list)
  (reduce (let ((pointer nil)
                (head    nil))
            (lambda (a d)
              (if pointer
                  (setf (cdr pointer) (list a)
                        pointer (cdr pointer))
                  (setf head (list a)
                        pointer head))
              head))
          list
          :initial-value '()
          :from-end t))


(defun revappend-reverse (list)
  (revappend list nil))

(defun nreconc-reverse (list)
  (nreconc (copy-list list) nil))



(mapc (lambda (fun)
          (assert (equal (mapcar fun '(() (a) (a b) (a b c) (a b c d) (a b c d e)))
                         '(nil (a) (b a) (c b a) (d c b a) (e d c b a)))))
      (list (function iterative-reverse-do)
            (function iterative-reverse-loop)
            (function naive-recursive-reverse)
            (function recursive-reverse)
            (function my-reverse)
            (function reduce-reverse-from-start)
            (function reduce-reverse-from-end)
            (function reduce-reverse-from-end-with-pointer)
            (function revappend-reverse)
            (function nreconc-reverse)))



;; A tail-recursive solution for the reversing of the list in place.
;; Notice that cl:nreverse may be implemented without reusing the
;; input list, cl:nreverse may just call cl:reverse.

(defun my-nreverse (list)
  (labels ((list-reverse (reverse list)
             (if (null list)
                 reverse
                 (let ((rest  (cdr list)))
                   (setf (cdr list) reverse)
                   (list-reverse list rest)))))
    (list-reverse nil list)))


;; (let* ((list     (list 1 2 3 4))
;;        (reversed (my-nreverse list)))
;;   (values list reversed))
;; --> (1)
;;     (4 3 2 1)



(defun nreconc-nreverse (list)
  (nreconc list nil))



(defun reduce-append (l1 l2)
  (reduce (function cons) l1 :initial-value l2 :from-end t))

;;;; THE END ;;;;
