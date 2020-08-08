#-(and) "

P28 (**) Sorting a list of lists according to length of sublists
    a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of
    this list according to their length. E.g. short lists first, longer lists later, or vice versa.
   
    Example:
    * (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
    ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))
   
    b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to
    sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done
    ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.
   
    Example:
    * (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
    ((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))
   
    Note that in the above example, the first two lists in the result have length 4 and 1, both lengths appear just
    once. The third and forth list have length 3 which appears twice (there are two list of this length). And
    finally, the last three lists have length 2. This is the most frequent length.
"


;;; a)

;; Simple direct solution, using Common Lisp.  This solution may have
;; a bad time complexity because sort may call the key several times.

(defun lsort (llist)
  (sort (copy-list llist) (function <) :key (function length)))


;; Simple direct solution, using Common Lisp, taking care of calling
;; length only once.  Moreover, we take care of using a vector as
;; temporary structure, so that we use less memory (and sorting
;; vectors may be faster):

(defun lsort (llist)
  (map 'list (function cdr)
       (sort (map 'vector (lambda (list) (cons (length list) list)) llist)
             (function <) :key (function car))))


;; (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
;; --> ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))



;;; b)

;; Simple direct solution, using Common Lisp:

(defun make-histogram (sequence &key (key (function identity)))
  (let ((table (make-hash-table)))
    (map nil (lambda (item) (incf (gethash (funcall key item) table 0))) sequence)
    table))

(defun lfsort (llist)
  (let* ((data  (map 'vector (lambda (list) (cons (length list) list)) llist))
         (histo (make-histogram data :key (function car))))
    (map 'list (function cdr)
         (sort data (function <) :key (lambda (item) (gethash (car item) histo))))))

;; (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
;; --> ((I J K L) (O) (A B C) (F G H) (D E) (D E) (M N))


;;;; THE END ;;;;
