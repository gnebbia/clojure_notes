#-(and) "
   
P23 (**) Extract a given number of randomly selected elements from a list.
    The selected items shall be returned in a list.
    Example:
    * (rnd-select '(a b c d e f g h) 3)
    (E D A)
   
    Hint: Use the built-in random number generator and the result of problem P20.
"

;; The word "extract" and P20, seem to hint that a given element may
;; be choosen only once.  Must the order be random too?




;; Recursive solution using the function remove-at of P20:

(defun rnd-select (list count)
  (if (zerop count)
      '()
      (let ((i (random (length list)))) ; lenght and elt are O(n) ==> rnd-select is O(n²).
        (cons (elt list i) (rnd-select (remove-at list (1+ i)) (1- count))))))


;; Iterative solution using the function remove-at of P20:

(defun rnd-select (list count)
  (loop
     :repeat count
     :for len :from (length list) :by -1
     :for i = (random len)
     :collect (elt list i) ; elt is O(n) ==> rnd-select is O(n²).
     :do (setf list (remove-at list (1+ i)))))


;; This solution extract the items in the same order than in list, by
;; precomputing a random bitmap.  The result is O(n):

(defun rnd-select (list count)
  (let ((len (length list)))
    (cond
      ((zerop count) '())  ; none selected.
      ((= count len) list) ; all selected.
      ((< 0 count len)
       (let ((bits (make-array len :element-type 'bit :initial-element 0)))
         (loop
            :while (plusp count)
            :for i = (random len)
            :do (when (zerop (aref bits i))
                  (setf (aref bits i) 1)
                  (decf count)))
         (loop
            :for item :in list
            :for indicator :across bits
            :when (plusp indicator)
            :collect item)))
      (t (error "Invalid count, must be between 0 and ~A" len)))))


;; This other solution remembers the indices selected, and avoids
;; reusing them.  This gives a random order, but the time complexity
;; is not deterministically bound, only statistically bound (assuming
;; a correct pseudo-random  generator):

(defun rnd-select (list count)
  (let ((len (length list)))
    (cond
      ((zerop count) '())  ; none selected.
      ((<= 1 count len)
       (loop
          :with indices = '()
          :with result  = '()
          :while (plusp count)
          :for i = (random len)
          :unless (member i indices)
          :do (progn
                (push i indices)
                (push (elt list i) result) ; elt is O(n) ==> rnd-select is O(n²).
                (decf count))
          :finally (return result)))
      (t (error "Invalid count, must be between 0 and ~A" len)))))


;; Other algorithms for random arrangements can be found in TAOCP...

;;;; THE END ;;;;
