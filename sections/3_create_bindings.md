# Creating Data

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


