# Clojure: Queue
Queues in clojure do not have a literal
representations as other collections we have explored.
They are not very much used, but can be useful from time
to time.
Queue remove data from the bottom and insert it on the top.

Queue is one of those cases where we may want to use mutable
state. Anyway they are rarely used in clojure, since there are
generally better options.
Anyway there may be cases where we need them.

Queues are generally commonly used in consumer-producer patterns.

# Create a Queue
(def queue clojure.lang.PersistentQueue/EMPTY)

# Add elements to the queue
(conj queue 1 2 3)

# Print a queue
(seq (conj queue 1 2 3))  ;; => (1 2 3)

# Queue access pattern

(conj queue 2)    ;; => (1 2 3 2)
(peek queue)      ;; gets the element that can be popped (3)
(pop queue)       ;; => 3


# Queue and mutable state

(def queue (atom clojure.lang.PersistentQueue/EMPTY))

(swap! queue conj 2)
(seq @queue)          ;; =>  (1 2)

(peek @queue)         ;; => 1
(swap! queue pop)
(seq @queue)          ;; => (2)
