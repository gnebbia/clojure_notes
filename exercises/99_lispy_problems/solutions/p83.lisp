#-(and)"
P83 (**) Construct all spanning trees

    Write a predicate s-tree(Graph,Tree) to construct (by
    backtracking) all spanning trees of a given graph. With this
    predicate, find out how many spanning trees there are for the
    graph depicted to the left. The data of this example graph can be
    found in the file p83.dat. When you have a correct solution for
    the s-tree/2 predicate, use it to define two other useful
    predicates: is-tree(Graph) and is-connected(Graph). Both are
    five-minutes tasks!
"

(load "p80.lisp")

(defparameter *p83-example-graph*
  (make-edge-graph '((a b) (a d)
                     (b c) (b e)
                     (c e)
                     (d e) (d f) (d g)
                     (e h)
                     (f g)
                     (g h))))




