# Clojure: Namespaces
Namespaces are used to keep things organized.

Clojure libs are made up of names and symbols associated with namespaces.


# Print the current namespace
*ns*


# Switch to another namespace
(ns another.namespace)
;; whatever we def will be within this new namespace


# Include an external library/module
We can include libs inside our namespace using three ways:
- using require and passing the lib name;
    (require 'clojure.set) ;; actually this specific library gets
                           ;;loaded automatically most of the times
- using require with ':as' to alias the namespace;
    (require '[food.favfoods :as ff])
- using require with ':refer' and ':all', this is generally a discouraged
    practice, since it easily creates namespace pollution;
    (:require [food.favfoods :refer :all])
    ;; an equivalent form that is not preferred is
    (:use [food.favfoods])



Note that although you can use require on its own, it is common to see
it nested within the ns, with a keyword and vector:
(ns wonderland
    (:require [food.favfoods :as ff]))
ff/fav-food
