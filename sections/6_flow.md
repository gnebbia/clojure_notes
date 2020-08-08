# Clojure: Flow

(true? true)   ;; true
(true? false)  ;; false

(false? false) ;; true
(false? true)  ;; false

(nil? nil)     ;; true
(nil? 1)       ;; false

(not true)     ;; false
(not false)    ;; true
(not nil)      ;; true
(not "hi")     ;; false


(= :drinkme :drinkme) ;; true
(= :drinkme 4)        ;; false

(= '(:drinkme :bottle) [:drinkme :bottle]) ;; true (this is collection equality)

(not= :drinkme :4) ;; true


## Logic Tests for Collections

(= '(:drinkme :bottle) [:drinkme :bottle]) ;; true (this is collection equality)

(empty? [:table :door])  ;; false
(empty? [])              ;; true
(empty? {})              ;; true
(empty? '())             ;; true

it is idiomatic to use `seq` instead of `(not (empty? x))`, like this:
(seq [])  ;; this returns nil which is false

(every? odd? [1 3 5])    ;; true
(every? odd? [1 2 3 5])  ;; false

We can also do the opposite, so taking a predicate for a collection
and returning false if it is true for any element in the collection.
Basically, if every element is false, the `not-any?` function returns
true:
(not-any? #(= % :drinkme) [:drinkme :poison])  ;; false
(not-any? #(= % :drinkme) [:poison :poison])   ;; true

We can also check if only some of the elements satisfy a condition
by using `some`, like this:
(some #(> % 3) [1 2 3 4 5 6 7]) ;; true

The `some` function can be used with a set to return the element,
or the first matching element of a specified sequence, like:
(some #{3} [1 2 3 4 5])     ;; 3
(some #{3 4 5} [1 2 3 4 5]) ;; 3


# Flow Control

## if

(if true
    "True"
    "False")

## if ... do
We can have if with multiple statements in the then/else
form.

(if true
    (do (println "Success!")    ;; prints "Success!"
        "By Zeus's hammer!")    ;; returns the string "By Zeus's hammer!"
    (do (println "Failure!")
        "By Aquaman's trident!"))


## when
The "When" operator is a combination of "if" and "do" without "else".
Use when if you want to do multiple things when some condition is true,
and you always want to return nil when the condition is false.

(when true
    (println "Success!")
    (comment "other stuff")
    "abra cadabra")


## if-let

Many times we find ourselves to save the result of a predicate, e.g.,

(let [is-bigger (> 5 3)]
    (if is-bigger
        "it's big!"
        "it's not that big"))

We can shortcut this using the "if-let" and doing:

(if-let [is-bigger (> 5 3)]
    "it's big!"
    "it's not that big!")

## when-let

Same thing happens with when.
(when-let [is-bigger (> 5 3)]
    "do stuff!")

## cond

The first true match stops the evaluation and returns.
If none of the tests match, a "nil" is returned.

(let [bottle "drinkme"]
    (cond
        (= bottle "poison")  "don't touch"
        (= bottle "drinkme") "grow!"
        (= bottle "empty")   "it's empty!"))

We can also use cond with a default clause to replace the if/elsif/else
formulation that you find in other languages:

(let [x 5]
    (cond
        (= x 10)     "just ten"
        (= x 100)    "hundred!"
        (= x 1000)   "it's thousand!"
        :else        "something else"))

There is nothing special about ":else" we could have used indeed something like
a string as "default", it is enough to have a true value.

## case

Case is a shortcut for the cond when:
- there is only one test value
- the comparison can be done with (=)
Notice that another difference wrt cond is that is not test
is matched instead of returning "nil" (as cond does), an error
is returned.

(let [bottle "drinkme"]
    (case bottle
        "poison"  "don't touchh"
        "drinkme" "grow smaller!"
        "empty"   "empty!"))

We can however add a default case also with "case" by doing:

(let [bottle "drinkme"]
    (case bottle
        "poison"  "don't touchh"
        "drinkme" "grow smaller!"
        "empty"   "empty!"
        "unknown"))
