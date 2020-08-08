#-(and) "   

Graphs

A graph is defined as a set of nodes and a set of edges, where each
edge is a pair of nodes.

There are several ways to represent graphs in Prolog. One method is to
represent each edge separately as one clause (fact). In this form, the
graph depicted below is represented as the following predicate:

[graph1]

edge(h,g).
edge(k,f).
edge(f,b).
...

We call this edge-clause form. Obviously, isolated nodes cannot be
represented. Another method is to represent the whole graph as one
data object. According to the definition of the graph as a pair of two
sets (nodes and edges), we may use the following Prolog term to
represent the example graph:

graph([b,c,d,f,g,h,k],[e(b,c),e(b,f),e(c,f),e(f,k),e(g,h)])

We call this graph-term form. Note, that the lists are kept sorted,
they are really sets, without duplicated elements. Each edge appears
only once in the edge list; i.e. an edge from a node x to another node
y is represented as e(x,y), the term e(y,x) is not present. The
graph-term form is our default representation. In SWI-Prolog there are
predefined predicates to work with sets.

A third representation method is to associate with each node the set
of nodes that are adjacent to that node. We call this the
adjacency-list form. In our example:

[n(b,[c,f]), n(c,[b,f]), n(d,[]), n(f,[b,c,k]), ...]

The representations we introduced so far are Prolog terms and
therefore well suited for automated processing, but their syntax is
not very user-friendly. Typing the terms by hand is cumbersome and
error-prone. We can define a more compact and \"human-friendly\"
notation as follows: A graph is represented by a list of atoms and
terms of the type X-Y (i.e. functor '-' and arity 2). The atoms stand
for isolated nodes, the X-Y terms describe edges. If an X appears as
an endpoint of an edge, it is automatically defined as a node. Our
example could be written as:

[b-c, f-c, g-h, d, f-b, k-f, h-g]

We call this the human-friendly form. As the example shows, the list
does not have to be sorted and may even contain the same edge multiple
times. Notice the isolated node d. (Actually, isolated nodes do not
even have to be atoms in the Prolog sense, they can be compound terms,
as in d(3.75,blue) instead of d in the example).

[graph2]

When the edges are directed we call them arcs. These are represented
by ordered pairs. Such a graph is called directed graph. To represent
a directed graph, the forms discussed above are slightly modified. The
example graph opposite is represented as follows:

Arc-clause form
    arc(s,u).
    arc(u,r).
    ...
   
Graph-term form
    digraph([r,s,t,u,v],[a(s,r),a(s,u),a(u,r),a(u,s),a(v,u)])
   
Adjacency-list form
    [n(r,[]),n(s,[r,u]),n(t,[]),n(u,[r]),n(v,[u])]

    Note that the adjacency-list does not have the information on
    whether it is a graph or a digraph.
   
Human-friendly form

    [s > r, t, u > r, s > u, u > s, v > u]

Finally, graphs and digraphs may have additional information attached
to nodes and edges (arcs). For the nodes, this is no problem, as we
can easily replace the single character identifiers with arbitrary
compound terms, such as city ('London',4711). On the other hand, for
edges we have to extend our notation. Graphs with additional
information attached to edges are called labelled graphs.

[graph3]

Arc-clause form
    arc(m,q,7).
    arc(p,q,9).
    arc(p,m,5).
   
Graph-term form
    digraph([k,m,p,q],[a(m,p,7),a(p,m,5),a(p,q,9)])
   
Adjacency-list form
    [n(k,[]),n(m,[q/7]),n(p,[m/5,q/9]),n(q,[])]

    Notice how the edge information has been packed into a term with
    functor '/' and arity 2, together with the corresponding node.
   
Human-friendly form
    [p>q/9, m>q/7, k, p>m/5]

The notation for labelled graphs can also be used for so-called
multi-graphs, where more than one edge (or arc) are allowed between
two given nodes.
"



#-(and) "

P80 (***) Conversions

    Write predicates to convert between the different graph
    representations.  With these predicates, all representations are
    equivalent; i.e.  for the following problems you can always pick
    freely the most convenient form.  The reason this problem is rated
    (***) is not because it's particularly difficult, but because it's
    a lot of work to deal with all the special cases.

"



#-(and) "

A similar set of representations are possible in lisp too.  As always,
hiding the representation behind an abstraction will allow to
implement generic algorithms, and to change the representation at
will.  We may even design an abstraction allowing to change the
representation on the fly, eg. between two phases of a processing, to
provide better algorithmic complexities.

To easily write and print graphs, we'll use a s-exp which must be a
list containing either isolated nodes (non-cons objects), or lists of
two or more elements (from-node to-node [:key value ...]) representing
each edge or arc.

We could easily write a function to map more sugary sexp syntax to
this form, or even a reader macro parsing a syntax with all the
intricacy wanted, but it's hardly worth the pain.

We'll also allow property lists to store any kind of attributes to the
arcs or edges.
"


;;; Graph classes

(defclass graph ()
  ((representation :initarg :representation
    :documentation "The actual representation of the graph."))
  (:documentation "
This abstract class represents a graph, and is the superclass of a
directed-graph and undirected-graph that can be represented in several
ways.
"))

(defclass undirected-graph (graph)
  ()
  (:documentation "
Undirected graphs can have only representations with edges.
"))

(defclass attributes ()
  ((property-list :initform '()
                  :accessor property-list :initarg :property-list
                  :accessor properties    :initarg :properties)))


(defclass edge (attributes)
  ((nodes :accessor edge-nodes :initarg :nodes))
  (:documentation "
An undirected edge. The order of the two nodes in the edge-nodes list
is irrelevant.
"))

(defmethod print-object ((self undirected-graph) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (let ((rep-name  (class-name (class-of (slot-value self 'representation)))))
     (format stream "as a~:[~;n~] ~A" (find (char (string rep-name) 0) "AEIOUY") rep-name))
    (format stream " with ~A node~:*~P and ~A edge~:*~P"
            (length (nodes self)) (length (edges self))))
  self)


(defgeneric edges-with-node (graph node)
  (:documentation "Returns a list of the edges in GRAPH associating the given NODE.")
  (:method ((g graph) node) (edges-with-node (slot-value g 'representation) node)))



(defclass directed-graph (graph)
  ()
  (:documentation "
Undirected graphs can have only representations with arcs.
"))

(defclass arc (attributes)
  ((from :accessor arc-from :initarg :from)
   (to   :accessor arc-to   :initarg :to))
  (:documentation "
A directed arc, from the FROM node to the TO node.
Note: the API allow for unidrected
"))

(defmethod print-object ((self directed-graph) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (let ((rep-name  (class-name (class-of (slot-value self 'representation)))))
     (format stream "as a~:[~;n~] ~A" (find (char (string rep-name) 0) "AEIOUY") rep-name))
    (format stream " with ~A node~:*~P and ~A arc~:*~P"
            (length (nodes self)) (length (arcs self))))
  self)


(defgeneric arcs-from-node (graph node)
  (:documentation "Returns a list of the arcs in GRAPH from the  NODE.
\(the adjacency list).")
  (:method ((g graph) node) (arcs-from-node (slot-value g 'representation) node)))

(defgeneric arcs-to-node (graph node)
  (:documentation "Returns a list of the arcs in GRAPH to the  NODE.")
  (:method ((g graph) node) (arcs-to-node (slot-value g 'representation) node)))







(defclass graph-representation ()
  ()
  (:documentation "An abstract graph representation."))

(defclass undirected-graph-representation ()
  ()
  (:documentation "An abstract undirected graph representation."))

(defclass directed-graph-representation ()
  ()
  (:documentation "An abstract directed graph representation."))



;;; Generic functions
;; We define here the fundamental operations for graph
;; representations, as generic functions.  A method is defined on
;; graph, that just forwards the call to the graph representation.

(defgeneric nodes (gr)
  (:documentation "Returns the list of nodes in the graph or graph representation")
  (:method ((g graph)) (nodes (slot-value g 'representation))))

(defgeneric add-node (gr node)
  (:documentation "Adds a new node to the graph or graph representation.
Return NODE.")
  (:method ((g graph) node) (add-node (slot-value g 'representation) node)))

(defgeneric remove-node (gr node)
  (:documentation "
If NODE is a node of the graph or graph representation, then remove
it, as well as all arcs connecting it.  Return NODE.")
  (:method ((g graph) node) (remove-node (slot-value g 'representation) node)))



(defgeneric edges (gr)
  (:documentation "
Returns the list of edges in the undirected graph or graph
representation.")
  (:method ((g undirected-graph)) (edges (slot-value g 'representation))))

(defgeneric add-edge-between-nodes (gr from to &key &allow-other-keys)
  ;; Notice we leave ADD-EDGE to name a generic function of two arguments: (gr edge)
  ;; Optional key arguments may be defined for additionnal
  ;; initializers for edges (such as weights, etc).
  (:documentation "
Adds a new edge the graph or graph representation, between the FROM
and the TO node.  If the graph or graph representation is undirected,
then two arcs are added, from FROM to TO and from TO to FROM.  If
either FROM or TO is not a node of GR, then it's added before.
Return the new EDGE.")
  (:method ((g undirected-graph) from to &rest args &key &allow-other-keys)
    (apply (function add-edge-between-nodes) (slot-value g 'representation) from to args)))

(defgeneric remove-edge (gr edge)
  (:documentation "
If EDGE is an edge of the graph or graph representation,then remove it.
Return EDGE.")
  (:method ((g undirected-graph) edge) (remove-edge (slot-value g 'representation) edge)))




(defgeneric arcs (gr)
  (:documentation "Returns the list of arcs in the graph or graph representation.
If the graph or graph representation is undirected, then each edge produces two arcs.")
  (:method ((g directed-graph))
    (arcs (slot-value g 'representation)))
  (:method ((g undirected-graph))
    (mapcan (lambda (edge)
              (destructuring-bind (left right) (edge-nodes edge)
                (list (make-instance 'arc :from left  :to right :properties (properties edge))
                      (make-instance 'arc :from right :to left :properties (properties edge)))))
            (edges (slot-value g 'representation)))))

(defgeneric add-arc-between-nodes (gr from to &key &allow-other-keys)
  ;; Notice we leave ADD-ARC to name a generic function of two arguments: (gr arc)
  ;; Optional key arguments may be defined for additionnal
  ;; initializers for arcs (such as weights, etc).
  (:documentation "
Adds a new arc the graph or graph representation, between the FROM
and the TO node.  If either FROM or TO is not a node of GR,
then it's added before.  Return the new ARC.")
  (:method ((g directed-graph) from to &rest args &key &allow-other-keys)
    (apply (function add-arc-between-nodes) (slot-value g 'representation) from to args)))

(defgeneric remove-arc (gr arc)
  (:documentation "
If ARC is an arc of the graph or graph representation,then remove it.
The nodes are not changed.  Return ARC.")
  (:method ((g directed-graph) arc) (remove-arc (slot-value g 'representation) arc)))


(defgeneric to-sexp (object)
  (:documentation "
Returns a sexp representing the graph.
The sexp should be accepted by the method FROM-SEXP
of the same graph class.
"))

(defgeneric from-sexp (object sexp)
  (:documentation "
Replaces the graph nodes and edges with the data from the given SEXP.
Returns GR.
"))




(defun nodes-and-links-to-sexp (nodes links)
  (flet ((nodes-from-links (links)
           (mapcan (lambda (link) (list (first link) (second link))) links)))
    (append (set-difference nodes (nodes-from-links links)) links)))

(defmethod to-sexp ((self edge))
  (concatenate 'list (edge-nodes self) (properties self)))

(defmethod to-sexp ((self arc))
  (concatenate 'list (list (arc-from self) (arc-to self)) (properties self)))

(defmethod to-sexp ((g directed-graph))
  (nodes-and-links-to-sexp (nodes g) (mapcar (function to-sexp) (arcs g))))

(defmethod to-sexp ((g undirected-graph))
  (nodes-and-links-to-sexp (nodes g) (mapcar (function to-sexp) (edges g))))



(defmethod clear-representation ((g graph))
  (setf (slot-value g 'representation) (make-instance (class-of (slot-value g 'representation)))))

(defmethod parse-graph-sexp ((g graph) sexp add-link)
  (let ((rep (clear-representation g)))
    (loop
       :for item :in sexp
       :do (if (consp item)
               (apply add-link rep item)
               (add-node rep item))
       :finally (return rep))))

(defmethod from-sexp ((g undirected-graph) sexp)
  (setf (slot-value g 'representation)
        (parse-graph-sexp g sexp (function add-edge-between-nodes)))
  g)

(defmethod from-sexp ((g directed-graph) sexp)
  (setf (slot-value g 'representation)
        (parse-graph-sexp g sexp (function add-arc-between-nodes)))
  g)



;; We'd want to
;; (define-modify-macro deletef (element list) delete)
;; but the order of the argument is not consistent.

(defmacro deletef (item sequence-place &rest args &key key test test-not)
  (declare (ignore key test test-not))
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion sequence-place)
    (when (cdr store-vars)
      (error "Cannot DELETE from a place with multiple values."))
    `(let* (,@(mapcar (function list) vars vals)
            (,(car store-vars) ,reader-form))
       (setf ,(car store-vars) (delete ,item ,(car store-vars) ,@args))
       ,writer-form)))




;;; Edge list representation
;;; In this representation we only keep a list of links.

(defclass edge-list-representation (undirected-graph-representation)
  ((edges  :accessor edges :initarg :edges  :initform '())))

(defmethod add-edge-between-nodes ((gr edge-list-representation) from to &rest properties &key &allow-other-keys)
  (let ((edge (make-instance 'edge
                  :nodes (list from to)
                  :properties properties)))
    (push edge (edges gr))
    edge))

(defmethod remove-edge ((gr edge-list-representation) edge)
  (deletef edge (edges gr))
  edge)

(defmethod nodes ((gr edge-list-representation))
  (delete-duplicates (loop
                        :for edge :in (edges gr)
                        :for nodes = (edge-nodes edge)
                        :collect (first nodes) :collect (second nodes))))

(defmethod add-node ((gr edge-list-representation) node)
  (declare (ignore gr node))
  (error "Cannot add isolated nodes to a graph represented by a list of edges."))

(defmethod remove-node ((gr edge-list-representation) node)
  (setf (edges gr) (delete-if (lambda (edge) (member node (edge-nodes edge))) (edges gr)))
  node)

(defmethod edges-with-node ((gr edge-list-representation) node)
  (remove-if-not (lambda (edge) (member node (edge-nodes edge))) (edges gr)))


(defmethod arcs-from-node ((gr edge-list-representation) from)
  (mapcar (lambda (edge)
            (make-instance 'arc :from from :to (first (remove from (edge-nodes edge)))))
          (edges-with-node gr from)))

(defmethod arcs-to-node ((gr edge-list-representation) to)
  (mapcar (lambda (edge)
            (make-instance 'arc :to to :from (first (remove to (edge-nodes edge)))))
          (edges-with-node gr to)))


;;; Edge list and nodes representation
;;; In this representation in addition to the list of edge, we
;;; maintain a list of nodes, so we may have isolated nodes too.

(defclass edge-and-node-list-representation (edge-list-representation)
  ((nodes :accessor nodes :initarg :nodes :initform '())))

(defmethod add-edge-between-nodes ((gr edge-and-node-list-representation) from to &key &allow-other-keys)
  (add-node gr from)
  (add-node gr to)
  (call-next-method))

(defmethod add-node ((gr edge-and-node-list-representation) node)
  (pushnew node (nodes gr))
  node)

(defmethod remove-node ((gr edge-and-node-list-representation) node)
  (remove node (nodes gr))
  (call-next-method))




;;; adjacency list representation
;;; In this representation, we have a hash-table mapping from nodes to
;;; lists of attributed links to nodes.  This allow for directed graphs.
;;; Notice that each node is present in the hash-table as a key, so
;;; isolated nodes are easily represented.

(defclass link (attributes)
  ((node :accessor link-node  :initarg :node)))

(defclass adjacency-list-representation (directed-graph-representation)
  ((adjacency-list :initform (make-hash-table)
                   :reader adjacency-list)))


(defmethod nodes ((gr adjacency-list-representation))
  (let ((nodes '()))
    (maphash (lambda (from adjacents)
               (declare (ignore adjacents))
               (push from nodes))
             (adjacency-list gr))
    nodes))

(defmethod add-node ((gr adjacency-list-representation) node)
  (unless (gethash node (adjacency-list gr))
    (setf (gethash node (adjacency-list gr)) '()))
  node)

(defmethod remove-node ((gr adjacency-list-representation) node)
  (let ((al (adjacency-list gr)))
    (when (remhash node al)
      (maphash (lambda (from adjacents)
                 ;; I assume it's faster to call (setf gethash)
                 ;; than to call member or find.
                 (setf (gethash from al) (delete node adjacents :key (function link-node))))
               al)))
  node)


(defun make-adjacency-list-arcs-from (from)
  (lambda (to)
    (make-instance 'arc
        :from from
        :to (link-node to)
        :properties (copy-list (properties to)))))


(defmethod arcs ((gr adjacency-list-representation))
  (let ((arcs '()))
    (maphash (lambda (from adjacents)
               (setf arcs (nconc (mapcar (make-adjacency-list-arcs-from from)
                                         adjacents) arcs)))
             (adjacency-list gr))
    arcs))

(defmethod add-arc-between-nodes ((gr adjacency-list-representation) from to &rest properties &key &allow-other-keys)
  (add-node gr from)
  (add-node gr to)
  (pushnew (make-instance 'link :node to :properties properties) (gethash from (adjacency-list gr)))
  (make-instance 'arc
      :from from
      :to to
      :properties (copy-list properties)))

(defmethod remove-arc ((gr adjacency-list-representation) arc)
  (deletef arc (gethash (arc-from arc) (adjacency-list gr)))
  arc)

(defmethod arcs-from-node ((gr adjacency-list-representation) from)
  (mapcar (make-adjacency-list-arcs-from from)
          (gethash from (adjacency-list gr))))

(defmethod arcs-to-node ((gr adjacency-list-representation) to)
  (let ((arcs '()))
    (maphash (lambda (from adjacents)
               (when (member to adjacents)
                 (setf arcs (nconc (funcall (make-adjacency-list-arcs-from from) to) arcs))))
             (adjacency-list gr))
    arcs))

;;; 

(defun make-edge-graph (data)
  (from-sexp (make-instance 'undirected-graph
                 :representation (make-instance 'edge-list-representation))
             data))

(defun make-edge-and-node-graph (data)
  (from-sexp (make-instance 'undirected-graph
                 :representation (make-instance 'edge-and-node-list-representation))
             data))

(defun make-adjacency-list-graph (data)
  (from-sexp (make-instance 'directed-graph
                 :representation (make-instance 'adjacency-list-representation))
             data))


(defun set-equal-p (a b)
  (and (subsetp a b :test (function equal))
       (subsetp b a :test (function equal))))

(defun test/to-sexp ()
  (dolist (test '(()
                  (a b c)
                  ((a b) (b c))
                  ((b c) (f c) (g h) d (f b) (k f) (h g))
                  ((s r) t (u r) (s u) (u s) (v u))
                  ((p q :weight 9) (m q :weight 7) k (p m :weight 5))))
    (assert (set-equal-p test (to-sexp (make-edge-and-node-graph  test))))
    (assert (set-equal-p test (to-sexp (make-adjacency-list-graph test)))))
  (dolist (test '(()
                  ((a b) (b c))
                  ((b c) (f c) (g h) (f b) (k f) (h g))
                  ((s r) (u r) (s u) (u s) (v u))
                  ((p q :weight 9) (m q :weight 7) (p m :weight 5))))
    (assert (set-equal-p test (to-sexp (make-edge-graph  test)))))
  :success)

;;  (test/to-sexp)


;; Converting from one graph representation to another can be realized with:
;;  (make-...-graph (to-sexp original-graph))
;; or use copy-from to replace the contents of the current graph with
;; those of the other graph:

(defmethod copy-from ((g graph) (other graph))
  "Make G a graph equal to OTHER"
  (clear-representation g)
  ;; Just out of lazyness, we go thru sexps.
  (from-sexp (slot-value g 'representation) (to-sexp other))
  ;; if a faster conversion is required, we could get (nodes other)
  ;; and (edges other) or (arcs other) and loop on them to add them to
  ;; the target graph.
  g)


;;;; THE END ;;;;
