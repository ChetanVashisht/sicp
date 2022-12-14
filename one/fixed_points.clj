(ns one.fixed-points
  (:use [commons])
  (:use [clojure.math]))

(defn good-enough? [a b]
  (< (abs (- a b)) 0.0001))

(defn search [f neg-point pos-point]
  (let [midpoint (average neg-point pos-point)
        fm (f midpoint)]
    (cond
      (zero? fm) midpoint
      (good-enough? neg-point pos-point) midpoint
      (pos? fm) (recur f neg-point midpoint)
      :else (recur f midpoint pos-point))))

(defn polynomial [x] (+ (* x x) (* -3 x) 2))

(comment
  (polynomial 1)
  (polynomial 2)
  (polynomial 0)
  (polynomial 1.5))

(search polynomial 1.5 4)

(defn half-interval-method [f a b]
  (let [a-val (f a)
        b-val (f b)]
    (cond
      (and (neg? a-val) (pos? b-val)) (search f a b)
      (and (pos? a-val) (neg? b-val)) (search f b a)
      (zero? a-val) a
      (zero? b-val) b
      :else (throw (AssertionError. "Incorrect input, both values have the same sign")))))

;; (half-interval-method polynomial 5 0)
;; ↪ Execution error (AssertionError) at one.fixed-points/half-interval-method (form-init12860298277056988051.clj:29).
;;    Incorrect input, both values have the same sign
(half-interval-method polynomial 5 1.5)
;; ↪ 2.000011444091797

(defn fixed-point
  ([f] (fixed-point f 1.0))
  ([f x]
   (let [fx (f x)]
     (cond
       (good-enough? fx x) x
       :else (recur f fx)))))

(fixed-point cos 1.0)
(fixed-point #(+ (sin %) (cos %)) 1.0)

;; This is a fixed point for y
;; y = 1/2 (y + x/y)
(defn square-root [y]
  (fixed-point #(average % (/ y %)) 1.0))

(square-root 9)


;; Ex 1.35
(defn polynomial-phi [x] (+ 1 (/ 1 x)))
(fixed-point polynomial-phi 1.0)

;; Ex 1.36
(defn fixed-point
  ([f] (fixed-point f 1.0))
  ([f x]
   (let [fx (f x)]
     (do
       (println x fx)
       (cond
         (good-enough? fx x) x
         :else (recur f fx))))))

(defn polynomial-xx [x] (/ (log 1000) (log x)))

(fixed-point polynomial-xx 2.0)
;; ↪ 4.555493957531389

;; This is a fixed point for x
;; x = 1/2 (x + ln(y)/ln(x))
(defn polynomial-xx-with-damping [y]
  (fixed-point #(average % (/ (log y) (log %))) 2.0))

(polynomial-xx-with-damping 1000)
