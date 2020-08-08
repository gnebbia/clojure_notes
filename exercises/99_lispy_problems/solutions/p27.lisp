#-(and) "

P27 (**) Group the elements of a set into disjoint subsets.

    a) In how many ways can a group of 9 people work in 3 disjoint
    subgroups of 2, 3 and 4 persons? Write a function that generates
    all the possibilities and returns them in a list.
   
    Example: * (group3 '(aldo beat carla david evi flip gary hugo
    ida)) ( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
    ... )
   
    b) Generalize the above predicate in a way that we can specify a
    list of group sizes and the predicate will return a list of
    groups.
   
    Example: * (group '(aldo beat carla david evi flip gary hugo ida)
    '(2 2 5)) ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
    ... )
   
    Note that we do not want permutations of the group members;
    i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO)
    ...). However, we make a difference between ((ALDO BEAT) (CARLA
    DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).
   
    You may find more about this combinatorial problem in a good book
    on discrete mathematics under the term \"multinomial
    coefficients\".
"   





(defun group3 (set)
  (group set '(2 3 4)))


(defun group (set sizes)
  (cond
    ((endp sizes)
     (error "Not enough sizes given."))
    ((endp (rest sizes))
     (if (= (first sizes) (length set))
         (list (list set))
         (error "Cardinal mismatch |set| = ~A ; required ~A" (length set) (first sizes))))
    (t
     (mapcan (lambda (combi)
               (mapcar (lambda (group) (cons combi group))
                       (group (set-difference set combi) (rest sizes))))
             (combinations (first sizes) set)))))


;; (map nil 'print (group3 '(aldo beat carla david evi flip gary hugo ida)))
;; (map nil 'print (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5)))

;;;; THE END ;;;;
