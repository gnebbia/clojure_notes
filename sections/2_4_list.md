# Clojure: Lists
Clojure implements lists as linear linked lists.
Lists are not very much used as a collection in clojure.
Although we could say that all clojure code is made out
of lists.
They tend to not be very much used because they are linear
in terms of element access.

# Create a list
()
'(1 2 3 4)
'(1 2 :a :b)
(list 1 2 3 4)

;; create a non-evaluated list
'(1 (+ 1 1) 3 4)      ;; => (1 (+ 1 1) 3 4) 

;; create an evaluated list
(list 1 (+ 1 1) 3 4)  ;; => (1 2 3 4)

# Adding element to a list
(cons 0 (list 1 2 3)) ;; => (0 1 2 3)

# Count a list (constant time by caching the value)
(count (cons 0 (cons 1 (cons 2 (cons 3 nil)))))  ;; => 4

# Insert stuff into list
(into () [1 2 3])  ;; => (3 2 1)
