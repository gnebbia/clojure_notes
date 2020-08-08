# Vectors
[1 2 "jam" :soccer]

## Create a vector
[1 2 3 :jam "hello"]
(conj [:toast :bread] :jam)
(vector 1 2 3 4)


Most common methods on vectors are:
- first
- rest
- last
- count (count elements of a vector)
- contains? (says if the index is present in the vector) (RE-READ this is confusing)
- vec (transforms a collection into a vector)

## Get vector length
(def v [1 2 3 2]
(count v)  ;; => 4

## Convert to vector (vec)
(vec (list 1 2 3 4))  ;; => [ 1 2 3 4 ]
(vec {:a 1 :b 1})  ;; => [[:a 1] [:b 2]]


### Get nth element or nil (get)
(def v [:a :b :c :d :e :f :g])

(get v 3)    ;; => :e
(get v 1000) ;; => nil
(get v -1)   ;; => nil

### Get nth element or error (nth)
(def v [:a :b :c :d :e :f :g])

(nth v 3)    ;; => :e
(nth v -1)   ;; error

### Equality checks

Equality works as expected, we can also compare a vector
with other collections.

(= [1 2 3 4] (list 1 2 3 4))  ;; => true

(= [1 2 3 4] [1 2 3 (* 2 2)]) ;; => true


### Getting subvectors
(def v [:a :b :c :d :e :f :g])

(subvec v 0 3)  ;; => [:a :b :c]

### Using vectors as stacks (conj, pop, peek)

(def v [1 2 3 4 5])
(conj v 6) ;; => [1 2 3 4 5 6]
(pop v)    ;; => [1 2 3 4]
(peek v)   ;; => 5


### Using vectors as functions
Although not very popular (as with hashmaps), this is possible
in clojure.

([:a :b :c :d] 0) ;; => :a
;; this is equivalent to nth
;; this is very useful in map functions
;; instead of doing this:
(map #(nth v %) [1 2 3 4 5])

;; we can just do this
(map v [1 2 3 4 5])
