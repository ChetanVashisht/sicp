(ns number-theory)

(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (mod a b))))

(gcd 40 206)

(defn square [x] (* x x))

(defn divides? [a b] (= 0 (mod a b)))

(comment
  (divides? 10 5)

  (find-divisor 35 2)
  (smallest-divisor 49)
  (smallest-divisor 199)
  (smallest-divisor 1999)
  (smallest-divisor 19999))

(defn next [d]
  (+ d (if (zero? (mod d 2)) 1 2)))

(comment
  (next 3)
  (next 5)
  (next 2))

(defn square [x] (* x x))

(defn divides? [a b] (= 0 (mod a b)))

(defn- find-divisor [n d]
  (cond
    (> (square d) n) n
    (divides? n d) d
    :else (recur n (next d))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn is-prime? [n]
  (= (smallest-divisor n) n))

(comment
  (is-prime? 101))

(defn get-prime-taken-time [lower]
  (time
   (->>
    (+ lower 1000)
    (range lower)
    (filter is-prime?)
    (take 5))))

(get-prime-taken-time 1000)
;; "Elapsed time: 0.005972 msecs"
(get-prime-taken-time 10000)
;; "Elapsed time: 0.006457 msecs"
(get-prime-taken-time 100000)
;; "Elapsed time: 0.006431 msecs"
(get-prime-taken-time 1000000)
;; "Elapsed time: 0.006914 msecs"
(get-prime-taken-time 10000000)
;; "Elapsed time: 0.006883 msecs"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn expmod [base exp n]
  (cond
    (= exp 0) 1
    (even? exp) (mod (square (expmod base (/ exp 2) n)) n)
    :else (mod (* base (expmod base (- exp 1) n)) n)))

(comment
  (expmod 2 6 10))

(defn fermat-test [n]
  (let [try-it (fn [a] (= (expmod a n n) a))]
    (try-it (+ 1 (rand-int n)))))

(fermat-test 43)
(fermat-test 561)

(defn fast-prime [n times]
  (cond
    (= 0 times) true
    (fermat-test n) (recur n (- times 1))
    :else false))

(defn fast-prime? [n]
  (fast-prime n 5))

(comment
  (fast-prime? 561)
  (fast-prime? 19999))

(defn get-prime-taken-time-fermat [lower]
  (time
   (->>
    (+ lower 1000)
    (range lower)
    (filter fast-prime?)
    (take 5))))

(get-prime-taken-time-fermat 1000)
;; "Elapsed time: 0.005972 msecs"
(get-prime-taken-time-fermat 10000)
;; "Elapsed time: 0.006457 msecs"
(get-prime-taken-time-fermat 100000)
;; "Elapsed time: 0.006431 msecs"
(get-prime-taken-time-fermat 1000000)
;; "Elapsed time: 0.006914 msecs"
(get-prime-taken-time-fermat 10000000)
;; "Elapsed time: 0.006883 msecs"
