(ns two.procedual-representation
  (:use [commons :refer :all])
  (:use clojure.test)
  (:use [clojure.math :refer :all]))

;; Ex 2.4

(defn cons [x y]
  (fn [m] (m x y)))

(defn car [z]
  (z (fn [p q] p)))

(defn cdr [z]
  (z (fn [p q] q)))

(testing "car cons cdr"
  (is (= 3 (car (cons 3 4))))
  (is (= 4 (cdr (cons 3 4)))))

;; Ex 2.5
(defn cons [a b]
  (* (pow 2 a) (pow 3 b)))

(defn pow-of
  ([n z] (pow-of n z 0))
  ([n z ans]
   (if (not (zero? (rem z n))) ans
       (pow-of n (/ z n) (+ 1 ans)))))

(testing "pow-of"
  (is (= 5 (pow-of 2 32)))
  (is (= 4 (pow-of 3 81))))

(defn car [z]
  (pow-of 2 z))

(defn cdr [z]
  (pow-of 3 z))

(testing "car cons cdr"
  (is (= 3 (car (cons 3 4))))
  (is (= 4 (cdr (cons 3 4)))))

;; Ex 2.6
