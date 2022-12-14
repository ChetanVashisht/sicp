(ns one.procedures-as-return-values
  (:use [commons :refer :all])
  (:use [clojure.math])
  (:require [one.fixed-points :refer [fixed-point]]))

(defn average-damp [f]
  (fn [x] (average x (f x))))

(comment
  (average-damp square))

(defn square-root [x]
  (fixed-point (average-damp (fn [y] (/ x y))) 1.0))

(comment
  (square-root 4.0))

(defn derivative [g]
  (let [dx 0.0001]
    (fn [x] (/ (- (g (+ x dx)) (g x)) dx))))

(comment
  ((derivative #(* 10 % %)) 1))

(defn newtonws-transform [f]
  (fn [x] (- x (/ (f x) ((derivative f) x)))))

(defn newtonws-method
  ([g] (newtonws-method g 1.0))
  ([g guess] (fixed-point (newtonws-transform g) guess)))

(defn square-root-newton [x]
  (newtonws-method (fn [y] (- (square y) x))))

(square-root-newton 9)

(defn cube-root-newton [x]
  (newtonws-method (fn [y] (- (cube y) x)) 50 ))

(cube-root-newton 27)

;; Ex 1.40
(defn cubic [a b c] (fn [x] (+ (* x x x) (* a x x) (* b x) (* c))))

(defn cubic-root [a b c]
  (newtonws-method (cubic a b c)))

(cubic-root 0 0 0)

;; Ex 1.41

(defn apply-twice [f]
  (fn h[x] (f (f x))))

(((apply-twice (apply-twice apply-twice)) inc) 5)
;; ↪ 21

;; This is madness, you expect the answer to be 13, but's 21
;; Somehow the function squares instead of linearly applies

;; Ex 1.42
(defn compose [f g]
  (fn [x] (f (g x))))

((compose square inc) 6)
;; ↪ 49

;; Ex 1.43
(defn repeated
  ([f n] (repeated f n identity))
  ([f n ans]
   (if (= 0 n) ans
       (recur f (minus1 n) (compose f ans)))))

((repeated square 2) 5)
;; ↪ 625

;; Ex 1.44
(defn smooth [f]
  (let [dx 0.001]
    (fn [x] (average (f (minus1 x)) (f x) (f (plus1 x))))))

((smooth sin) 0.50)

(defn n-fold-smoothened [f n]
  (repeated (smooth f) n))

((n-fold-smoothened sin 20) PI)

;; Ex 1.45
(defn fourth-root [x]
  (fixed-point (average-damp (average-damp (fn [y] (/ x (cube y)))))))

(fourth-root 81)
;; 3.0064616 2.9935799
;; 2.9935799 3.0064616
;; 3.0064616 2.9935799
;; 2.9935799 3.0064616
;; 3.0064616 2.9935799
;; 2.9935799 3.0064616

(defn sixth-root [x]
  (fixed-point (average-damp (average-damp (fn [y] (/ x (pow y 5)))))))

(comment
  (int (/ (log 17) (log 2))))

(defn nth-root [x n]
  (let [no-of-damps (int (/ (log n) (log 2)))
        f (fn [y] (/ x (pow y (minus1 n))))]
    (fixed-point
     ((repeated average-damp no-of-damps) f))))

(nth-root 32 5)

;; Ex 1.46
(defn iterative-improvement
  [good-enough improvement]
  (fn [guess]
    (if (good-enough guess)
      guess
      (recur (improvement guess)))))

(defn fixed-point-iterative [f guess]
  (let [good-enough? (fn [x] (< (abs (- (f x) x)) 0.001))
        improvement (fn [x] (f x))]
    ((iterative-improvement good-enough? improvement) guess)))

(defn polynomial-xx [x] (/ (log 1000) (log x)))

(fixed-point-iterative polynomial-xx 3.0)

(defn square-root-iterative [x]
  (let [guess 1.0
        f (fn [y] (/ x y))
        good-enough? (fn [x] (< (abs (- (f x) x)) 0.001))
        improvement (fn [y] (average y (f y)))]
    ((iterative-improvement good-enough? improvement) guess)))

(square-root-iterative 9.0)
