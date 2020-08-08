#-(and) "

P24 (*) Lotto: Draw N different random numbers from the set 1..M.
    The selected numbers shall be returned in a list.
    Example:
    * (lotto-select 6 49)
    (23 1 17 33 21 37)
   
    Hint: Combine the solutions of problems P22 and P23.
"

;; Note: for this problem any solution of p23 is valid: the order of
;; lotto draws is irrelevant, therefore we can just chose combinations
;; in order.


(defun lotto-select (selection set-size)
  (rnd-select (range 1 set-size) selection))


;;;; THE END ;;;;

