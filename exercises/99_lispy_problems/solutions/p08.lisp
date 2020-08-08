#-(and) "
   
P08 (**) Eliminate consecutive duplicates of list elements.
    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
   
    Example:
    * (compress '(a a a a b c c a a d e e e e))
    (A B C A D E)

"

;; Nice recursive solution:

(defun compress (list)
  (labels ((compress-run (item list)
             (cond ((null list)              (list item))
                   ((eql item (first list))  (compress-run item (rest list)))
                   (t                        (cons item (compress-run (first list) (rest list)))))))
   (cond
     ((null list)          list)
     ((null (rest list))   list)
     (t                    (compress-run (first list) (rest list))))))


;; Iterative solution:

(defun compress (list)
  (cond
    ((null list)          list)
    ((null (rest list))   list)
    (t  (loop
           :with result = '()
           :with item = (first list)
           :for other :in (rest list)
           :do (unless (eql item other)
                 (push item result)
                 (setf item other))
           :finally (push item result) (return (nreverse result))))))



;; Smartass solution, using Common Lisp reduce:

(defun compress (list)
  (reduce (lambda (item result)
            (cond
              ((endp result)             (list item))
              ((eql (first result) item) result)
              (t                         (cons item result))))
          list
          :from-end t
          :initial-value '()))


;; Without :from-end, we need a reverse, and notice the order of the
;; arguments to the function:

(defun compress (list)
  (nreverse (reduce (lambda (result item)
                      (cond
                        ((endp result)             (list item))
                        ((eql (first result) item) result)
                        (t                         (cons item result))))
                    list
                    :initial-value '())))


;;;; THE END ;;;;
