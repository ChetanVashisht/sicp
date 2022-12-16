(ns commons)

(defn square [x] (* x x))

(defn average [& args] (float (/ (apply + args) (count args))))

(defn abs [a] (if (neg? a) (- a) a))

(defn minus1 [a] (- a 1))

(defn plus1 [a] (+ a 1))

(defn cube [a] (* a a a))

(defn gcd [a b]
  (if (= b 0) a
      (recur b (mod a b))))

;; (defn cons [x y]
;;   (fn [m] (m x y)))

;; (defn car [z]
;;   (z (fn [p q] p)))

;; (defn cdr [z]
;;   (z (fn [p q] q)))

(defn cons [item coll] (conj coll item))

(defn car [x] (first x))

(defn cdr [z] (rest z))

(defn cadr [z] (first (rest z)))

(defn caddr [z] (first (rest (rest z))))

(defn null? [x] (empty? x))
