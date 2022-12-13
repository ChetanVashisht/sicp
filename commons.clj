(ns commons)

(defn average [a b] (/ (+ a b) 2))

(defn abs [a] (if (neg? a) (- a) a))
