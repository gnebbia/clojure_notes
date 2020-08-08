#-(and) "

P10 (*) Run-length encoding of a list.
    Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
   
    Example:
    * (encode '(a a a a b c c a a d e e e e))
    ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

"


;; Nice recursive solution, using the same design pattern as in P09:

(defun encode (list)
  (labels ((encode-run (element count list)
             (cond
               ((null list) (list (list count element)))
               ((eql element (first list)) (encode-run element (1+ count) (rest list)))
               (t (cons (list count element) (encode-run (first list) 1 (rest list)))))))
    (if (null list)
        '()
        (encode-run (first list) 1 (rest list)))))


;; Nice, functional solution, reusing functions from p09, uses O(n+r) space:

(defun encode (list)
  (mapcar (lambda (group) (list (length group) (first group)))
          (group list)))


;; Smartass solution, using Common Lisp reduce:

(defun encode (list)
  (reduce (lambda (item result)
            (print (list item result))
            (cond
              ((endp result)                      (list (list 1 item)))
              ((eql (second (first result)) item) (cons (list (1+ (first (first result))) item)
                                                        (rest result)))
              (t                                  (cons (list 1 item) result))))
          list
          :from-end t
          :initial-value '()))


;; Smartass solution, using Common Lisp reduce, and a closure; more
;; efficient since we don't call cons twice at each step, but
;; inelegant, since we have to add the last result as an afterthought:

(defun encode (list)
  (when list
    (let ((count     0)
          (last-item nil))
      (let ((tail-result (reduce (lambda (item result)
                                   (cond
                                     ((zerop count)
                                      (setf count 1
                                            last-item item)
                                      result)
                                     ((eql item last-item)
                                      (incf count)
                                      result)
                                     (t
                                      (prog1 (cons (list count last-item) result)
                                        (setf count 1
                                              last-item item)))))
                                 list
                                 :from-end t
                                 :initial-value '())))
        (cons (list count last-item) tail-result)))))


;; Iterative solution, uses only O(r) space:

(defun encode (list)
  (when list
    (loop
       :with count = 0
       :with last-item = nil
       :with result = '()
       :for item :in list
       :do (cond
             ((zerop count)        (setf count 1
                                         last-item item))
             ((eql item last-item) (incf count))
             (t                    (push (list count last-item) result)
                                   (setf count 1
                                         last-item item)))
       :finally (when (plusp count)
                  (push (list count last-item) result))
       (return (nreverse result)))))


;;;; THE END ;;;;
