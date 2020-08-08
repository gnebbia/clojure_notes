# Clojure: Symbols

By default we are in the "user" namespace, so once we define
a symbol called "const1" it can be called by:
- const1 
- user/const1

Remember to use:
- "def", to create global symbols
- "let", to create temporary bindings


# Create a global binding
(def user "User1")
;; -> #'user/developer

# Create temporary binding
(let [user "UserTest"]
    user)
;; -> "UserTest"

# Create multiple temporary bindings
(let [developer "Alice in Wonderland"
      rabbit "White Rabbit"]
    [developer rabbit])


