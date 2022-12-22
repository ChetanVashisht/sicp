(ns two.7-conventional-interfaces
  (:use [commons :refer :all])
  (:use clojure.test)
  (:use [clojure.math :refer :all]))

(defn enumerate-tree [tree]
  (cond
    (null? tree) nil
    (!pair? tree) (list tree)
    :else (append (enumerate-tree (car tree))
                  (enumerate-tree (cdr tree)))))

(testing "Testing enumerate-tree"
  (is (= '(1 2 3 4 5) (enumerate-tree (list 1 2 (list 3 4) (list 5))))))

(defn sum-of-odd-squares [tree]
  (reduce + 0 (map square (filter odd? (enumerate-tree tree)))))

(testing "Testing sum-of-odd-squares"
  (is (= 35 (sum-of-odd-squares (list 1 2 (list 3 4) (list 5))))))

(defn fib
  ([n] (fib n 1 0))
  ([n a b]
   (if (= 0 n) a
       (fib (- n 1) (+ a b) a))))

(defn even-fibs [n]
  (reduce conj [] (filter even? (map fib (range n)))))

(even-fibs 10)

;; Ex 2.33
(defn mapper [p sequence]
  (reduce (fn [x y] (cons (p y) x)) nil sequence))

(mapper square (list 1 2 4))

(defn appender [seq1 seq2]
  (reduce conj seq2 (reverse seq1)))

(appender (list 1 2 34) (list 4 5 7))

(defn len [seq]
  (reduce (fn [x y] (+ x 1)) 0 seq))

(len (list 3 4 5))

;; Ex 2.34
(defn horner-eval [x coeffs]
  (reduce (fn [other-terms coeff]
            (+ (* other-terms x) coeff))
          0 coeffs))

;; For y = x^5 + 5x^3 + 3x + 1
(horner-eval 2 (list 1 0 5 0 3 1))

;; Ex 2.35
(defn count-leaves [tree]
  (reduce (fn [x y] (+ x 1)) 0 (map identity (enumerate-tree tree))))

(testing "Testing count-leaves"
  (is (= 5 (count-leaves (list 1 2 (list 3 4) (list 5))))))

(comment
  (map first (list (list 1 2 3) (list 4 5 6)))
  (map rest (list (list 1 2 3) (list 4 5 6))))

;; Ex 2.36
;; (defn accumulate-n [op init seqs]
;;   (if (null? (car seqs)) nil
;;       (cons (reduce op init (map first seqs))
;;             (accumulate-n op init (map rest seqs)))))

(testing "Testing accumulate-n"
  (is (= '(4 10 18) (accumulate-n * 1 (list (list 1 2 3) (list 4 5 6))))))

;; Ex 2.37
(def mvec list)

(def mvector? list?)

(defn dot-product [v w]
  (reduce + 0 (map * v w)))

(testing "Testing dot-product"
  (is (= 14 (dot-product (mvec 1 2 3) (mvec 1 2 3)))))

(defn matrix*vector [m v]
  (map (fn [row] (dot-product row v)) m))

(testing "Testing matrix-into-vector"
  (is (= '(26 43 60) (matrix*vector (mvec (mvec 1 2 3) (mvec 2 3 4) (mvec 3 4 5)) (mvec 8 9 0)))))

(defn- transpose-internal [mat row]
  (if (null? row)
    mat
    (cons
     (cons (car row)
           (if (null? (car mat)) [] (car mat)))
     (transpose-internal (cdr mat) (cdr row)))))

(defn transpose [m]
  (map (partial apply list) (reduce transpose-internal (list) m)))

(testing "Testing transpose"
  (is (= '((1 20 300) (2 30 400) (3 40 500)) (transpose (mvec (mvec 1 2 3) (mvec 20 30 40) (mvec 300 400 500))))))

(defn matrix*matrix [m n]
  (let [cols (transpose n)]
    (map
     (fn [row] (map (fn [col] (dot-product row col)) cols))
     m)))

(matrix*matrix
 (mvec (mvec 1 2 3) (mvec 20 30 40) (mvec 300 400 500))
 (mvec (mvec 1 2 3) (mvec 20 30 40) (mvec 300 400 500)))

(defn transpose2 [m]
  (accumulate-n conj nil m))

(transpose2 (mvec (mvec 1 2 3) (mvec 20 30 40) (mvec 300 400 500)))

;; Ex 2.38
(defn fold-left [op initial sequence]
  (let* [iter (fn [result others]
                (if (null? others)
                  result
                  (recur (op result (car others)) (cdr others))))]
    (iter initial sequence)))

(defn fold-right [op initial sequence]
  (if (null? sequence)
    initial
    (op (car sequence)
        (fold-right op initial (cdr sequence)))))

(fold-right / 1 (list 1 2 4))
(fold-left / 1 (list 1 2 4))

(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

;; Ex 2.39
(defn rev-right [sec]
  (fold-right (fn [x y] (conj y x)) [] sec))

(defn rev-left [sec]
  (fold-left conj nil sec))

(rev-right (list 1 2 3 4))
(rev-left (list 1 2 3 4))
