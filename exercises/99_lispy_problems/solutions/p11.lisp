#-(and) "

P11 (*) Modified run-length encoding.
    Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into
    the result list. Only elements with duplicates are transferred as (N E) lists.
   
    Example:
    * (encode-modified '(a a a a b c c a a d e e e e))
    ((4 A) B (2 C) (2 A) D (4 E))
"


;; Simple functional solution:

(defun encode-modified (list)
  (mapcar (lambda (item)
            (if (= 1 (first item))
                (second item)
                item))
          (encode list)))

;; See p13 for an iterative solution.

;;;; THE END ;;;;
