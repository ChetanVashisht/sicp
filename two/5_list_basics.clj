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
