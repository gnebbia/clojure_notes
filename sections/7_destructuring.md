# Destructuring
Destructuring allows us to assign named bindings for the elements
in entities such as vectors, maps and so on.
This commonly happens both when we use "let" or whenever we define
functions.

Let's start from a very simple example:
(let [[color size] ["blue" "small"]]
    (str "The " color " door is " size))

In this case color is assigned with "blue" and
size gets "small".

Let's see another example with a nested vector:

(let [[color [size]] ["blue" ["very small"]]]
    (str "The " color " door is " size)) ;; -> "The blue door is very small"

Destructuring can also keep the whole initial data structure as
binding, by using the ":as" keyword. For example

(let [[color [size] :as original] ["blue" ["small"]]]
    {:color color :size size :original original})
    ;; -> {:color "blue", :size "small", :original ["blue" ["small"]]}

Destructuring can also be done with maps.
For example:
(let [{flower1 :flower1 flower2 :flower2}
      {:flower1 "red" :flower2 "blue"}]
    (str "The flowers are " flower1 " and " flower2))
       ;; -> "The flowers are red and blue"

We can also specify default values to use for the keys if they are not found
in the map with ":or". For example:
(let [{flower1 :flower1 flower2 :flower2 :or {flower2 "missing"}}
      {:flower1 "red"}]
    (str "The flowers are " flower1 " and " flower2))
    ;; -> "The flowers are red and missing"
