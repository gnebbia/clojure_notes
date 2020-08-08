#-(and)"

P06 (*) Find out whether a list is a palindrome.
    A palindrome can be read forward or backward; e.g. (x a m a x).

"

;; The simple solution, 2n steps in the worst case (a palindrome), n
;; in the best case (not a palindrome), uses O(n) space:

(defun palindromep (list)
  (equal list (reverse list)))


;; A more complex solution, 3n/2 steps in the worst case, n steps in
;; the best case, use O(n) space:


(defun reversed-spine (list)
  "Returns a list containing each cons cells of the list in reversed order.
Example:  (reversed-spine '(1 2 3)) --> '(#1=(3) #2=(2 . #1#) #1=(1 . #2#))
"
  (loop
     :with result = '()
     :for cell :on list
     :do (push cell result)
     :finally (return result)))


(defun palindromep (list)
  (loop
     :for left  :on list
     :for right :in (reversed-spine list)
     :until (or (eq left right) (eq (cdr left) right))
     :unless (eql (car left) (car right)) :do (return nil)
     :finally (return t)))


;; A tail-recursive solution, O(n) steps, and uses O(n/2) space on the
;; stack, and O(n/2) space:

(defun palindromep (list)
  (labels ((palstep (slow fast reve)
             (cond
               ((endp fast)        (equal slow reve))
               ((endp (rest fast)) (equal (rest slow) reve))
               (t                  (palstep (rest slow)
                                            (rest (rest fast))
                                            (cons (first slow) reve))))))
    (palstep list list '())))




;; A smartass solution, O(n) steps, O(n) space (but with a smaller
;; constant than the other solutions).

(defun palindromep (list)
  (loop
     :with data = (coerce list 'vector)
     :for i :from 0
     :for j :from (1- (length data)) :by -1
     :while (< i j)
     :always  (eql (aref data i) (aref data j))))


#-(and)
(mapcar (function palindromep) '((x a m a x)
                                 (x a m m a x)
                                 (x)
                                 ()
                                 (x a m b x)))
;; --> (T T T T NIL)


;;;; THE END ;;;;
