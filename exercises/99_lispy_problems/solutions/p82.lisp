#-(and) "
P82 (*) Cycle from a given node

    Write a predicate cycle(G,A,P) to find a closed path (cycle) P
    starting at a given node A in the graph G. The predicate should
    return all cycles via backtracking.

"
(load "p80.lisp")
(load "p81.lisp")


(defmethod cycle ((g graph) node)
  "Return a list of all the cycles passing by NODE."

  )


;;;; THE END ;;;;
