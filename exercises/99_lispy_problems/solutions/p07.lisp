#-(and) "

P07 (**) Flatten a nested list structure.
  Transform a list, possibly holding lists as elements into a `flat'
  list by replacing each list with its elements (recursively).
   
    Example:
    * (my-flatten '(a (b (c d) e)))
    (A B C D E)
   
    Hint: Use the predefined functions list and append.
"

;; Bad recursive solution, using list and append as hinted, O(n²):

(defun my-flatten (list)
  (cond
    ((null list) list)
    ((atom list) (list list))
    ((list (first list))  (append (my-flatten (first list))
                                  (my-flatten (rest  list))))
    (t                    (append (list (first list))
                                  (my-flatten (rest  list))))))


;; Good, recursive solution, using an accumulator, O(n):

(defun my-flatten (list)
  (let ((result '()))
    (labels ((collect          (item) (push item result))
             (collected-result ()     (nreverse result))
             (walk-list (sublist)
               (dolist (item sublist)
                 (if (listp item)
                     (walk-list item)
                     (collect   item)))))
      (walk-list list)
      (collected-result))))


;; On results overflowing the cache size, we may want to avoid the nreverse:

(defun my-flatten (list)
  (let* ((result (cons 'header nil)) ; this cons avoids a test in collect
         (tail   result))
    (labels ((collect          (item) (setf (cdr tail) (cons item nil)
                                            tail       (cdr tail)))
             (collected-result ()     (cdr result))
             (walk-list (sublist)
               (dolist (item sublist)
                 (if (listp item)
                     (walk-list item)
                     (collect   item)))))
      (walk-list list)
      (collected-result))))


;; An iterative solution:

(defun my-flatten (tree)
  "
RETURN: A list containing all the elements of the `tree'.
"
  (loop
     :with result = nil
     :with stack = nil
     :while (or tree stack)
     :do (cond
           ((null tree)
            (setq tree (pop stack)))
           ((atom tree)
            (push tree result)
            (setq tree (pop stack)))
           ((listp (car tree))
            (push (cdr tree) stack)
            (setq tree (car tree)))
           (t
            (push (car tree) result)
            (setq tree (cdr tree))))
     :finally (return (nreverse result))))


;;; THE END ;;;;
