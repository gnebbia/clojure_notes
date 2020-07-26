# Create Functions

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



