# Collections
All collections are immutable and persistent.
- Immutable = value does not change;
- Persistent = means that these collections will do smart
               creations of new versions of themselves by
               using structural sharing;

Common collections are:
- lists
- vectors
- maps
- sets


## Lists vs Vectors

Lists are fine when you just want to get an element off the top of the list. But what if you
have a collection of things and you want to get the element that is right in the middle? In
other words, what if you need index access? This is when you need a vector.

Although you can use nth and last on lists as well as vectors, you will get better index
access performance with vectors. This is because a list needs to start at the beginning to
find its way to the element it wants


# List
'(1 2 "jam" :soccer)

## Create a list
'(1 2 3 4 5)
(list 1 2 3 4 5)
(cons 5 '())
(cons 5 nil)
(cons 4 (cons 5 nil)) ;; '(4 5)


Most common methods on list are:
- first
- rest
- count

# Vectors
[1 2 "jam" :soccer]

## Create a vector
[1 2 3 :jam "hello"]
(conj [:toast :bread] :jam)


Most common methods on vectors are:
- first
- rest
- last
- nth
- count

## Conj and Cons
You can think of conj as being an "insert somewhere" operation, and cons
as being an "insert at the head" operation. On a list, it is most logical
to insert at the head, so conj and cons are equivalent in this case.

Conj in general will make the most smart thing for a specific data
structure, i.e., insert at the head for a list and at the bottom for
a vector.


# Maps

Maps are the classical key-value data structure.

## Create a Map
{:jam1 "strawberry" :jam2 "blackberry"}

## Get value from map
(get {:jam1 "strawberry" :jam2 "blackberry"} :jam2)
;; or
(:jam2 {:jam1 "strawberry" :jam2 "blackberry"})


## Get value from map or return default value
(get {:jam1 "strawberry" :jam2 "blackberry"} :jam3 "not found")

## Get all keys of a map
(keys {:jam1 "strawberry" :jam2 "blueberry"})

## Get all values of a map
(values {:jam1 "strawberry" :jam2 "blueberry"})

## Update a map (creates a new one because of immutability)
(assoc {:jam1 "black" :jam2 "blue"} :jam1 "red")

## Remove a key/value pair
(dissoc {:jam1 "black" :jam2 "blue"} :jam1)

## Merge maps
(merge {:jam1 "red" :jam2 "black"}
{:jam1 "orange" :jam3 "red"}
{:jam4 "blue"})
;; -> {:jam4 "blue", :jam3 "red", :jam2 "black", :jam1 "orange"}

# Sets

## Create a set
\#{:red :blue :black :grey}

## Operations on set
(clojure.set/union #{:r :b :w} #{:w :p :y})
(clojure.set/difference #{:r :b :w} #{:w :p :y})
(clojure.set/intersection #{:r :b :w} #{:w :p :y})

(contains? #{:rabbit :door :watch} :rabbit) ;; -> true
(contains? #{:rabbit :door :watch} :jam) ;; -> false


You can convert another type of collection to a set using the set
function. This is useful for using set operations on things like vectors.

## Convert other data structure to sets
(set [:rabbit :rabbit :watch :door])
;; -> #{:door :watch :rabbit}
(set {:a 1 :b 2 :c 3})
;; -> #{[:c 3] [:b 2] [:a 1]}

## Access fields within sets
(get #{:rabbit :door :watch} :rabbit)
;; -> :rabbit
(get #{:rabbit :door :watch} :jar)
;; -> nil

We can also access it directly using the keyword:
(:rabbit #{:rabbit :door :watch})
;; -> :rabbit

## Add elements to a set
(conj #{:rabbit :door} :jam)
;; -> #{:door :rabbit :jam}

## Remove elements from a set
(disj #{:rabbit :door} :door) ;; -> #{:rabbit}
