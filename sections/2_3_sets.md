# Clojure Sets

Sets are collections containing only unique elements.

# Create a Set
#{} ;; => #{}
#{1 2 3 :a :b} ;; => #{1 3 2 :b :a}

(hash-set 1 2 3 4)  ;; => #{1 4 3 2}


# Convert other collection to a set
(set [1 2 3 4])   ;; => #{1 4 3 2}

(set (for [x (range 10)]
        ;;;
        x))

# Adding elements to sets and checking equality
(def a "abc")             ;; => #{a b c}
(def s #{a})              ;; => #{a}

(identical? s s)          ;; => true
(identical? s (conj s a)) ;; => true

# Lookup by value
(get s "abc") ;;=> "abc"

# Count values
(set [1 2 3 4])   ;; => #{1 4 3 2}

(count s)         ;; => 4

# Removing an element
(disj s :elemtoremove)

# Containment check
(contains? s "abc")     ;; => true


# Containment check (multi-comparison)
(def vice-presidents #{"John" "Linda" "June" "Fred"})

;; check if the name is a vice-president,
;; this is very fast, is done in constant time
(defn vp? [name]
    (contains? vice-presidents name))

;; it goes further than that, we can define a list of attendance
(def attendance ["Eric" "John" "June" "Jane" "Laura"])

;; anche check which elements are in both sets by doing:
(filter vp? attendance)
;; or by using the set as a function:
(filter vice-presidents attendance)

