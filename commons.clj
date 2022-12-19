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

(defn cons [item others]
  (conj others item))

(def car first)

(def cdr rest)

(defn cadr [z] (first (rest z)))

(defn caddr [z] (first (rest (rest z))))

(defn null? [x] (empty? x))

(def pair? seq?)

(defn append [list1 list2]
  (if (empty? list1) list2
      (cons (car list1) (append (cdr list1) list2))))
