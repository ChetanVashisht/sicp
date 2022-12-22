(ns one.continued-fractions
  (:use [commons :refer :all])
  (:require [clojure.math]))

;; Ex 1.37

(defn continued-fractions
  ([n d k] (continued-fractions n d k 0.0))
  ([n d k ans]
   (if (= k 0)
     ans
     (let [numo (n k)
           deno (d k)
           ans (/ numo (+ deno ans))]
       (continued-fractions n d (- k 1) ans)))))

(let [f (fn [_] 1)]
  (continued-fractions f f 11))
;; => 0.6180555555555556

(defn continued-fractions-recursive
  [n d k]
  (if (= k 1)
    0.0
    (let [numo (n k)
          deno (n k)]
      (/ numo (+ deno (continued-fractions-recursive n d (minus1 k)))))))

(let [f (fn [_] 1)]
  (continued-fractions-recursive f f 12))
;; => 0.6180555555555556


;; Ex 1.38
(defn e []
  (+ 2
     (let [n (fn [_] 1)
           d (fn [i]
               (if (= 2 (mod i 3))
                 (* 2 (/ (+ 1 i) 3))
                 1))]
       (continued-fractions n d 12))))

(e)
;; => 2.7182818229439496


;; Ex 1.39
(defn tan-cc [x k]
  (let [n (fn [k] (if (= k 1) x (- (square x))))
        d (fn [k] (- (* 2 k) 1))]
    (continued-fractions n d k)))

(tan-cc 3 9)
;; => -0.14254657540455998
