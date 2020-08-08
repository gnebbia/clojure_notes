#-(and) "

P02 (*) Find the last but one box of a list.
    Example:
    * (my-but-last '(a b c d))
    (C D)
"

;; The nice, recursive solution:

(defun my-but-last (list)
  (cond
    ((endp list)                (error "Empty list"))
    ((endp (rest list))         (error "List too short"))
    ((endp (rest (rest list)))  list)
    (t                          (my-but-last (rest list)))))


;; The efficient, iterative solution:

(defun my-but-last (list)
  (cond
    ((endp list)                (error "Empty list"))
    ((endp (rest list))         (error "List too short"))
    (t  (loop
           :for result :on list
           :until (endp (rest (rest result)))
           :finally (return result)))))


;; The smartass, Common Lisp solution:

(defun my-but-last (list)
  (cond
    ((endp list)                (error "Empty list"))
    ((endp (rest list))         (error "List too short"))
    (t                          (last list 2))))




;; We test and signal an error when the list is too short (the
;; specifications asks for at least 2 boxes (cons cells).  We could
;; use CHECK-TYPE with a type specification corresponding to lists of
;; at least to cons cells:

(defun my-but-last (list)
  (check-type list (cons t (cons t t)))
  (if (endp (cddr list))
      list
      (my-but-last (cdr list))))


;; Now we may consider that checking the list at each recursive call
;; would be expensive (and useless).  Indeed, in general, checking the
;; arguments needs to be done only on public entry points (module
;; APIs), while internal functions can assume that their
;; preconditions, their arguments, and the global invariants of their
;; module are valid.  We may use LABELS to define internal recursive
;; functions (FLET if they're not recursive):

(defun my-but-last (list)
  (check-type list (cons t (cons t t)))
  (labels ((find-but-last (list)
             (if (endp (cddr list))
                 list
                 (find-but-last (cdr list)))))
    (find-but-last list)))

;; We are using endp to test and signal an error on dotted-lists such
;; as (a b c . d).  But the problem statement didn't strictly exclude
;; them.  One may argue that "list" covers dotted-lists, circular
;; lists as well as proper-lists.  Furthermore, the only constraint is
;; that there are two cons cells, nothing is said about the contents
;; of the CDR slot of the last cons cell.  Therefore we may use ATOM
;; to test for cdr of the last cons cell, or the negation, CONSP if we
;; exchange the branches of the IF:

(defun my-but-last (list)
  (check-type list (cons t (cons t t)))
  (labels ((find-but-last (list)
             (if (consp (cddr list))
                 (find-but-last (cdr list))
                 list)))
    (find-but-last list)))



;; Note: we would use check-type on functions close to the user,
;; typically, functions given user data, so that the user may be able
;; to substite a correct and meaningful value for a bad value.  But
;; functions in deeper code should probably directly signal an error
;; notably when they process massaged internal data that the user may
;; not recognize (and would be in pain to correct meaningfull).

;;;; THE END ;;;;
