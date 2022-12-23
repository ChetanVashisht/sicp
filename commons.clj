(ns commons
  (:use clojure.test))

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

(defmacro car [x]
  `(first ~x))

(defmacro cdr [x]
  `(next ~x))

(defn cadr [z] (first (rest z)))

(defn caddr [z] (first (rest (rest z))))

(defmacro null? [x]
  `(or (nil? ~x)
       (if (seq? ~x)
         (empty? ~x)
         false)))

(defmacro pair? [x] `(seq? ~x))

(defmacro !pair? [x]
  `(not (pair? ~x)))

(!pair? (list 1 2 3))

(null? (list 12 4))

(car (list 1 2 3))

(defn append [list1 list2]
  (if (null? list1) list2
      (cons (car list1) (append (cdr list1) list2))))

(testing "Testing append"
  (is (= '(1 2 3 3 5 6) (append (list 1 2 3) (list 3 5 6)))))


(defn flatmap [f seq]
  (reduce append nil (map f seq)))

(defn accumulate-n [op init seqs]
  (if (null? (car seqs)) nil
      (cons (reduce op init (map first seqs))
            (accumulate-n op init (map rest seqs)))))


(defn memq [item x]
  (cond
    (null? x) false
    (= item (car x)) x
    :else (recur item (cdr x))))
