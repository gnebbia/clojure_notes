# Clojure Hashmaps

A hashmap consists in a set of keys with associated values.
Clojure is very flexible for keys wrt other languages.
In fact, we can have complex keys of whatever type not only strings.
Hashmaps are the workhorse of clojure.

Hashmaps are generally used for:
- modeling entities (think about a person with name, id, age, mail,...)
- modeling an index (think about a squares hashmap 4 -> 16, 8 -> 64, ...)

We define hashmaps with curly braces.
{} ;; => {}

Example of hashmap:
{:a 1
 :b 2
 :c 3} => {:a 1, :b 2, :c 3}

We can have whatever key we want (also complex types,
e.g.:
{[1 2] :alive
 [3 3] :dead};; => {[1 2] :alive, [3 3] :dead}


## Create a hashmap
{:a 1
 :b 2
 :c 3}

(hash-map :a 1 :b 2 :c 3) => {:c 3, :b 2, :a 1}


## Add stuff into a collection
(into {}  [[:a 1] [:b 2] [:c 3]]) ;; => {:a 1, :b 2, :c 3}

A common scenario is adding stuff within a loop like this:
(into {} (for [k (range 10)]
        [k (* k k)]))       ;; => {0 0, 7 49, 1 1, 4 16, 6 36, 3 9, 2 4, 9 81, 5 25, 8 64}

## Keys and Values
(def h {:a 1, :b 2, :c 3})

(keys v) ;; => (:a :b :c)

(values v)

## Merge hashmaps

(merge {:a 1 :b 2 :c 3} {:d 4 :e 5})  ;; => {:a 1 :b 2 :c 3 :d 4 :e 5}

## Get a subset of a hashmap
(select-keys v [:a :b])  ;; => {:a 1 ;b 2}


## Get elements from hashmap
(get {:a :b 2} :a)  ;; => 1
(get {} :a)         ;; => nil


## Associate key to value (Insert new values in a hashmap)
(assoc {:a 1} :b 2 :c 3 "hello" "world") ;; => {:a 1, :b 2, :c 3, "hello" "world"}

## Dissociate a key (Remove a pair by key)
(dissoc v :a :b)

## Couting keys
(count v)  ;; => 3

## Equality
(= (hash-map :a 1 :b 2) (array-map :a 1 :b 2))  ;; => true

(= (hash-map :a 1 :b 2) (hash-map :b 2 :a 1))  ;; => true

## Does it contain a key?
(contains? v :a) ;; => true

## Hashmaps as functions
Hashmaps are much more used with respect to vectors as functions in
clojure.

(def squares {0 0, 7 49, 1 1, 4 16, 6 36, 3 9, 2 4, 9 81, 5 25, 8 64}
(map squares (range 6)) ;; => (0 1 4 9 16 25)


Another example:

(def days {0 :Sunday,
           1 :Monday,
           2 :Tuesday,
           3 :Wednesday,
           4 :Thursday,
           5 :Friday,
           6 :Saturday})

(def day-numbers [1 4 4 2 5 5 4 2])

(map days day-numbers)



