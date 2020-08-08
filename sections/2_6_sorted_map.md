# Sorted Map
This is like a hashmap, except that the keys are being sorted.


# Create a sorted map
(sorted-map :c 3 :b 2 :a 1) ;; => {:a 1 :b 2 :c 3}

# Define the sorting function

(sorted-map-by (comparator <) 0 :a 1 :b 2 :c) ;; => {0 :a 1 :b 2 :c}

(sorted-map-by (comparator >) 0 :a 1 :b 2 :c) ;; => {2 :c 1 :b 0 :a}

;; create a custom sorting function

(defn sort-by-name [a b]
    (compare (:name a) (:name b)))

(sorted-map-by sort-by-name {:name "Eric"} 1 {:name "John"} 3 {:name "Andy"} 2)
;; => {{:name "Andy"} 2, {:name "Eric"} 1, {:name "John"} 3}

# Inserting values into sorted-maps

(into (sorted-map) {:a 1 :c 2 :z 4 :d 5})
;; => {:a :c 2 :d 5 :z 4}
