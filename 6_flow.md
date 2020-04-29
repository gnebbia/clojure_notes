# Flow

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

The `some` function can be used with a set to returnn the element,
or the first matching element of a specified sequence, like:
(some #{3} [1 2 3 4 5])   ;; 3
(some #{3 4 5} [1 2 3 4 5]) ;; 3


## Flow Control
