#-(and)"
P84 (**) Construct the minimal spanning tree

    Write a predicate ms-tree(Graph,Tree,Sum) to construct the minimal
    spanning tree of a given labelled graph. Hint: Use the algorithm
    of Prim. A small modification of the solution of P83 does the
    trick. The data of the example graph to the right can be found in
    the file p84.dat.
"

(load "p80.lisp")

(defparameter *p84-example-graph*
  (make-weighted-edge-graph '((a b 5) (a d 3)
                              (b c 2) (b e 4)
                              (c e 6)
                              (d e 7) (d f 4) (d g 3)
                              (e h 5)
                              (f g 4)
                              (g h 1))))




