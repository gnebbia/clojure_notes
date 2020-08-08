# Clojure: Create Functions

# Function with no parameters
(defn follow-the-rabbit [] "Off we go!")
(follow-the-rabbit)

# Functions with parameters
(defn shop-for-jams [jam1 jam2]
    {:name "jam-basket"
     :jam1 jam1
     :jam2 jam2})
(shop-for-jams "strawberry" "marmalade")

# Anonymous Functions
(fn [] (str "Off we go" "!"))

# Call an Anonymous Function
((fn [] (str "Off we go" "!")))

# Anonymous Functions (Short-Hand)
(#(str "Off we go" "!"))

# Anonymous Functions with one parameter (Short-Hand)
(#(str "Off we go" "!" " - " %) "again")

# Anonymous Functions with multiple parameters (Short-Hand)
(#(str "Off we go" "!" " - " %1 %2) "again" "?")


# Currying in Clojure (partial)
Currying is used for partial appllication of functions, and is useful
whenever we want to partially apply a function without the need of
carrying around all the values when we have to call our function of
interest.

(defn multiply [fac1 fac2]
    (* fac1 fac2))

(def multiplier-3 (partial multiply 3))
(multiplier-3 4)       ;; => 12

# Combining functions in Clojure (comp)
We can combine multiple functions into one function using (comp).

The function (comp) creates a new function that combines other functions.
It takes any number of functions as its parameters and returns the
composition of these functions going from right to left.

Let's say we have two functions:
(defn toggle-grow [direction]
    (if (= direction :small) :big :small))

(defn oh-my [direction]
    (str "Oh my! You aree growing " direction))

We can compose these functions either by doing in the procedural way:
(oh-my (toggle-grow :small))

Or in a more functional way using (comp):
(defn surprise [direction]
    ((comp oh-my toggle-grow) direction))

(surprise :small)                           ;; => "Oh My! You are growing :big"

