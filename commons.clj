(ns commons)

(defn square [x] (* x x))

(defn average [& args] (float (/ (apply + args) (count args))))

(defn abs [a] (if (neg? a) (- a) a))

(defn minus1 [a] (- a 1))

(defn plus1 [a] (+ a 1))

(defn cube [a] (* a a a))
