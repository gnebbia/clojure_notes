#-(and) "

P13 (**) Run-length encoding of a list (direct solution).
    Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the
    sublists containing the duplicates, as in problem P09, but only count them. As in problem P11, simplify the
    result list by replacing the singleton lists (1 X) by X.
   
    Example:
    * (encode-direct '(a a a a b c c a a d e e e e))
    ((4 A) B (2 C) (2 A) D (4 E))
"

;; Iterative solution, uses only O(r) space:

(defun encode-modified (list)
  (let ((result    '())
        (count     0)
        (last-item nil))
    (labels ((collect-result ()
               (push (if (= 1 count)
                         last-item
                         (list count last-item))
                     result))
             (new-item (item)
               (setf count 1
                     last-item item))
             (same-item ()
               (incf count))
             (return-result ()
               (when (plusp count)
                 (collect-result))
               (nreverse result)))
      (dolist (item list  (return-result))
        (cond
          ((zerop count)        (new-item item))
          ((eql item last-item) (same-item))
          (t                    (collect-result)
                                (new-item item)))))))


;;;; THE END ;;;;
