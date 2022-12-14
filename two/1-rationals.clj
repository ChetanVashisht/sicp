(ns two.rationals
  (:use [commons :refer :all])
  (:require [clojure.math]))


(defn cons-cells [f l]
  #(if % f l))

(defn numer [r]
  (r true))

(defn denom [r]
  (r false))

;; Ex 2.1
(defn make-rational [n d]
  (let [g (gcd n d)]
    (if (zero? d)
      (throw (AssertionError. "Denominator can't be 0"))
      (cons-cells (/ n g) (/ d g)))))

(print-rational (make-rational 0 7))
(print-rational (make-rational 3 -6))
(print-rational (make-rational -3 6))
(print-rational (make-rational -3 -6))
(print-rational (make-rational 3 6))

(defn print-rational [x]
  (print (numer x) "/" (denom x) " "))

(defn add-rationals [x y]
  (make-rational (+ (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                 (* (denom x) (denom y))))

(defn subtract-rationals [x y]
  (make-rational (- (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                 (* (denom x) (denom y))))

(comment
  (let [x (make-rational 3 4)
        y (make-rational 3 4)]
    (print-rational (add-rationals x y))
    (print-rational (subtract-rationals x y))))

(defn multiply-rationals [x y]
  (make-rational (* (numer x) (numer y))
                 (* (denom x) (denom y))))

(defn divide-rationals [x y]
  (make-rational (* (numer x) (denom y))
                 (* (numer y) (denom x))))


(defn equals-rationals [x y]
  (= (* (numer x) (denom y)) (* (denom x) (numer y))))

(comment
  (let [x (make-rational 3 4)
        y (make-rational 3 8)]
    (print-rational (multiply-rationals x y))
    (print-rational (divide-rationals x y))
    (equals-rationals x y)))
