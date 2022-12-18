(ns two.5-list-basics
  (:use [commons :refer :all])
  (:use clojure.test)
  (:use [clojure.math :refer :all]))

(def list-1-4 (list 1 2 3 4))

(testing
    (is (= 1 (car list-1-4))))

(testing
    (is (= '(2 3) (cdr (list 1 2 3)))))

(defn list-ref [l n]
  (if (= 0 n)
    (car l)
    (list-ref (cdr l) (- n 1))))

(testing "Testing list-ref"
  (is (= 4 (list-ref (list 1 2 3 4) 3))))

(defn length [l]
  (let [len (fn [l a]
              (if (empty? l)
                a
                (recur (cdr l) (+ a 1))))]
    (len l 0)))

(testing "Testing something"
  (is (= 0 (length (list ))))
  (is (= 4 (length (list 1 2 3 4)))))

(defn append [list1 list2]
  (if (empty? list1) list2
      (cons (car list1) (append (cdr list1) list2))))

(testing "Testing expressions"
  (is (= '(1 2 34 3 4 5) (append (list 1 2 34) (list 3 4 5)))))

(testing "Testing expressions"
  (is (= true (null? (list ))))
  (is (= false (null? (list 1 2 34)))))

;; Ex 2.17
(defn last-pair [l]
  (if (null? (cdr l))
    (car l)
    (recur (cdr l))))

(comment

  (testing "Testing last-pair"
    (is (= nil (last-pair (list)))))
  (testing "Testing last pair"
    (is (= 34 (last-pair (list 23 72 149 34))))))

;; Ex 2.18
(defn reverse-list [l]
  (let [rev (fn [l rl]
              (if (null? l)
                rl
                (recur (cdr l) (cons (car l) rl))))]
    (rev l nil)))

(testing "Testing reverse-list"
  (is (= '(6 4 3 2 1) (reverse-list (list 1 2 3 4 6)))))

;; Ex 2.19
(defn coin-change-base
  ([total denominations] (coin-change-base total denominations (- (length denominations) 1)))
  ([total denominations index]
   (do
     (print total denominations index)
     (cond
       (zero? total) 1
       (neg? index) 0
       (neg? total) 0
       :else (let [value (list-ref denominations index)]
               (+ (coin-change-base (- total value) denominations index)
                  (coin-change-base total denominations (- index 1))))))))

(testing "Testing coin-change"
  (is (= 292 (coin-change-base 100 (list 50 25 10 5 1)))))

(defn coin-change [total denominations]
  (cond
    (zero? total) 1
    (neg? total) 0
    (null? denominations) 0
    :else (let [first-denomination car
                except-first-denomination cdr]
            (+ (coin-change total (except-first-denomination denominations))
               (coin-change (- total (first-denomination denominations)) denominations)))))

(testing "Testing coin-change"
  (is (= 292 (coin-change 100 (reverse-list (list 50 25 10 5 1))))))

;; Ex 2.20
(defn g [& w]
  (println w))

(defn same-parity [& l]
  (let [parity (mod (car l) 2)
        filt (fn [l parity out]
               (if (null? l)
                 out
                 (let [elem (car l)
                       qualifies? (= (mod elem 2) parity)]
                   (recur (cdr l) parity (if qualifies? (cons elem out) out)))))]
    (filt l parity [])))

(testing "Testing same-parity"
  (is (= '(10 2 16) (same-parity 10 2 3 45 16))))

(same-parity 10 2 3 45 16)
