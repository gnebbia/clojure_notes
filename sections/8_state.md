# Mutable State

We can change the state of something using:
- atoms
- refs
- agents

# Atoms

Atoms are designed to store the state of something that is
independent, meaning we can change the value of it independently
of changing any other state.

    (def who-atom (atom :caterpillar))
or 

    (def mlist-atom (atom '(1 2 3 4))

If we print the atom, we get a reference to the atom itself,
to get back the value stored by the atom we have to dereference
it by using the "@" symbol. For example:

    @who-atom
    ;; -> :caterpillar
    @mlist-atom
    ;; -> (1 2 3 4)


To change the state of an atom there are a couple of ways:
- reset!, just replace with another value
- swap!, replace with another value through a function


## Using reset!
One way is through the use of reset! which replaces
the old value with the new one. For example:

    (reset! who-atom :chrysalis)
    @who-atom
    ;; -> :chrysalis

## Using swap!

We can replace a value through the use of a function,
for example:

    (defn change [state]
        (case state
        :caterpillar :chrysalis
        :chrysalis :butterfly
        :butterfly))

    (swap! who-atom change)
    ;; -> :chrysalis



# Refs

We just saw that atoms were used for independent and synchronous state changes. What if
we have more than one thing that needs to change in a coordinated fashion? Take as an
example transferring money between two bank accounts. This is where Clojure’s refs
come in. They allow this coordinated shared state. What makes them different from atoms
is that you need to change their values within a transaction. Clojure uses something called
software transactional memory (STM) to accomplish this. Refs use this STM to coordinate
changes of state.

Let's see an example:

    (def alice-height (ref 3))
    (def right-hand-bites (ref 10))

    ;; just as with atoms we can dereference them to get the value
    @alice-height
    ;; -> 3

We can use "alter" to change values of ref, this is equivalent to
"swap" with atoms, in the sense that alter takes a function and a
variable to change.

    (defn eat-from-right-hand []
    (dosync (when (pos? @right-hand-bites)
       (alter right-hand-bites dec)
       (alter alice-height #(+ % 24)))))

    (eat-from-right-hand)

We can changee refs only within a "dosync".

