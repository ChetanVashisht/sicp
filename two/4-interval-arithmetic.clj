(ns two.interval-arithmetic
  (:use [commons :refer :all])
  (:use clojure.test)
  (:use [clojure.math :refer :all]))

;; Ex 2.7

(defn make-interval [a b]
  (cons a b))

(defn upper-bound [interval]
  (cdr interval))

(defn lower-bound [interval]
  (car interval))

(testing
    (let [interval (make-interval 4 10)]
      (do
        (is (= 4 (lower-bound interval)))
        (is (= 10 (upper-bound interval))))))

;; Ex 2.8
(defn add-intervals [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn multiply-intervals [x y]
  (let [lx (lower-bound x)
        ux (upper-bound x)
        ly (lower-bound y)
        uy (upper-bound y)
        p1 (* lx ly)
        p2 (* lx uy)
        p3 (* ux ly)
        p4 (* ux uy)]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn divide-intervals [x y]
  (let [ly (lower-bound y)
        uy (upper-bound y)]
    (cond
      (and (neg? ly) (pos? uy)) (throw (AssertionError. "Can't divide the interval as it crosses 0"))
      (and (pos? ly) (neg? uy)) (throw (AssertionError. "Can't divide the interval as it crosses 0"))
      (or (zero? ly) (zero? uy)) (throw (AssertionError. "Can't divide the interval as it crosses 0"))
      :else (multiply-intervals x
                                (make-interval (/ 1.0 (upper-bound y))
                                               (/ 1.0 (lower-bound y)))))))

(defn print-interval [interval]
  (println "*" (lower-bound interval) "-" (upper-bound interval) "*" (tolerance interval)))

(defn subtract-intervals [x y]
  (let [lx (lower-bound x)
        ux (upper-bound x)
        ly (lower-bound y)
        uy (upper-bound y)]
    (make-interval (- lx uy)
                   (- ux ly))))

(let [x (make-interval 4 10)
      y (make-interval 5 10)]
  (println "********")
  (print-interval x)
  (print-interval (add-intervals x y))
  (print-interval (subtract-intervals x y))
  (print-interval (multiply-intervals x y))
  (print-interval (divide-intervals x y)))

;; Ex 2.11
(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))

(defn center [interval]
  (average (lower-bound interval) (upper-bound interval)))

(defn width [interval]
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

(defn make-center-percent [c p]
  (let [w (* c p 0.01)]
    (make-center-width c w)))

(defn tolerance [interval]
  (* (/ (width interval) (center interval)) 100))

(let [interval (make-center-percent 3.5 10)]
  (print-interval interval)
  (println (center interval))
  (println (tolerance interval)))

(let [A (make-center-percent 20 2)
      B (make-center-percent 100 3)
      AA (divide-intervals A A)
      AB (divide-intervals A B)
      A*A (multiply-intervals A A)
      A*B (multiply-intervals A B)
      A+B (add-intervals A B)
      A+A (add-intervals A A)]
  (print-interval AA)
  (print-interval AB)
  (print-interval A*A)
  (print-interval A*B)
  ;; (print-interval (divide-intervals A*A A*B))
  )

(defn par1 [r1 r2]
  (divide-intervals (multiply-intervals r1 r2)
                    (add-intervals r1 r2)))

(defn par2 [r1 r2]
  (let [one (make-interval 1 1)]
    (divide-intervals one
                      (add-intervals (divide-intervals one r1)
                                     (divide-intervals one r2)))))

(let [r1 (make-center-percent 20 4)
      r2 (make-center-percent 200 1)]
  (print-interval (par1 r1 r2))
  (print-interval (par2 r1 r2)))
