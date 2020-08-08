# Clojure: Documentation

# Get an overview of basic help/doc functionalities
(help)

# List the available functions in a module/namespace
(dir clojure.string)
(dir clojure.java.io)

# Find documentation about a module
(doc clojure.test)

# Find documentation about a function
(doc clojure.java.io/copy)
(doc clojure.java.io/writer)

# Read source of a function (things can be found here when doc is poor)
(source inc)

# Apropos
(find-doc "string to search")

# Get Javadoc
(javadoc java-object-or-class)

# Get a List of Core Clojure Namespaces/Modules
(clojure.pprint/pprint (all-ns))
;; or with autocompletion
(doc clojure.<TAB><TAB>)
(doc java.<TAB><TAB>)


