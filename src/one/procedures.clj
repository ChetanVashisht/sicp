(ns procedures
  (:use [number-theory :only [gcd]]))

(defn sum
  ([term a next b] (sum term a next b 0))
  ([term a next b ans]
   (if (> a b)
     ans
     (recur term (next a) next b (+ ans (term a))))))

(defn cube [n] (* n n n))

(sum cube 0 inc 10)

(defn integral [f a b]
  (let [dx 0.001
        next (fn [a] (+ a dx))
        a (+ a (/ dx 2))]
    (* (sum f a next b) dx)))

(integral cube 0 1)
;; => 0.24999987500000073

(defn simpson-integral [f a b]
  (let [n 1000
        h (/ (- b a) n)
        next inc
        yk (fn [k] (f (+ a (* k h))))
        coeff (fn [k] (cond
                        (or (= 0 k) (= n k)) 1
                        (even? k) 2
                        :else 4))
        simpson (fn [k] (* (coeff k) (yk k)))]
    (* (/ h 3) (sum simpson 0 next n))))

(simpson-integral cube 0.0 1.0)
;; => 0.25000000000000006


(defn product
  ([f a next b] (product f a next b 1))
  ([f a next b ans]
   (if (> a b) ans
       (recur f (next a) next b (* ans (f a))))))

(defn factorial [n]
  (product identity 1 inc n))

(defn pi []
  (let [numer (fn [n] (+ n (if (odd? n) 1 2)))
        deno (fn [n] (+ n (if (odd? n) 2 1)))
        f (fn [n] (/ (numer n) (deno n)))]
    (* 4.0 (product-2 f 1 inc 1000))))

(pi)
;; => 3.143160705532266

(defn accumulate
  ([combiner ans term a next b]
   (if (> a b) ans
       (accumulate combiner (combiner (term a) ans) term (next a) next b))))

(defn product-2
  [f a next b]
  (accumulate * 1 f a next b))

(defn sum-2 [f a next b]
  (accumulate + 0 f a next b))

(defn filtered-accumulate [combiner filt ans f a next b]
  (let [term (f a)
        filtered-res (if (filt a) (combiner ans term) ans)]
    (if (> a b)
      ans
      (recur combiner filt filtered-res f (next a) next b))))



(defn square [x] (* x x))

(defn divides? [a b] (= 0 (mod a b)))

(defn- find-divisor [n d]
  (cond
    (> (square d) n) n
    (divides? n d) d
    :else (recur n (inc d))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn is-prime? [n]
  (= (smallest-divisor n) n))

(is-prime? 7)

(defn sum-of-squares-of-primes [a b]
  (filtered-accumulate + is-prime? 0 square a inc b))

(sum-of-squares-of-primes 1 5)


(defn product-all-coprimes [n]
  (let [coprime? (fn [m] (= (gcd n m) 1))]
    (filtered-accumulate * coprime? 1 identity 1 inc n)))

(product-all-coprimes 7)
