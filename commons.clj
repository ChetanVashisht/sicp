(ns commons)

(defn square [x] (* x x))

(defn average [a b] (/ (+ a b) 2))

(defn abs [a] (if (neg? a) (- a) a))

(defn minus1 [a] (- a 1))
