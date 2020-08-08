#-(and) "

P69 (**) Dotstring representation of binary trees

    We consider again binary trees with nodes that are identified by
    single lower-case letters, as in the example of problem P67. Such
    a tree can be represented by the preorder sequence of its nodes in
    which dots (.) are inserted where an empty subtree (nil) is
    encountered during the tree traversal. For example, the tree shown
    in problem P67 is represented as 'abd..e..c.fg...'. First, try to
    establish a syntax (BNF or syntax diagrams) and then write a
    predicate tree-dotstring/2 which does the conversion in both
    directions. Use difference lists.
"

;; See full-preorder and binary-tree-from-full-preorder in p68.lisp.

;;;; THE END ;;;;
