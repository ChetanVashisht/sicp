(ns two.6-trees
  (:use [commons :refer :all])
  (:use clojure.test)
  (:use [clojure.math :refer :all]))

(defn count-leaves [x]
  (cond
    (nil? x) 0
    (not (pair? x)) 1
    :else (+ (count-leaves (car x))
             (count-leaves (cdr x)))))


;; Ex 2.26
(def x (list 1 2 3))
(def y (list 4 5 6))

(testing "Testing append"
  (is (= '(1 2 3 4 5 6) (append x y))))
(testing "Testing cons"
  (is (= '((1 2 3) 4 5 6) (cons x y))))
(testing "Testing list"
  (is (= '((1 2 3) (4 5 6)) (list x y))))

;; Ex 2.27
(def x (list (list 1 2) (list 3 4)))

x
;; ↪ ((1 2) (3 4))

(defn deep-reverse [l rl]
  (if (zero? (count l))
    rl
    (recur (cdr l)
           (cons
            (if (seq? (car l))
              (deep-reverse (car l) (list))
              (car l))
            rl))))

(deep-reverse x (list))
;; ↪ ((3 4) (1 2))


(testing "Testing deep-reverse"
  (is (= '(4 3 2 1) (deep-reverse (list 1 2 3 4) (list)))))
(testing "Testing deep-reverse"
  (is (= '((4 3) (2 1)) (deep-reverse (list (list 1 2) (list 3 4)) (list)))))

;; Ex 2.28

(defn fringe [l]
  (let [d (car l)]
    (if (null? l) nil
        (append (if (seq? (car l))
                  (fringe (car l))
                  (list (car l)))
                (fringe (cdr l))))))

(fringe (list (list 4 5 6) (list 2 3) 4))

(fringe (list x x))

;; Ex 2.29
(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

(defn left-branch [mobile]
  (first mobile))

(defn right-branch [mobile]
  (fnext mobile))

(defn weight? [structure]
  (not (pair? structure)))

(defn weight [branch]
  branch)

(defn fringe [l]
  (let [d (car l)]
    (if (null? l) nil
        (append (if (seq? (car l))
                  (fringe (car l))
                  (list (car l)))
                (fringe (cdr l))))))

(defn total-weight [mobile]
  (+ (if (weight? mobile)
       (weight mobile)
       (total-weight (left-branch mobile)))
     (total-weight (right-branch mobile))))

(testing "Testing right-branch"
  (is (= '(7 8) (right-branch (make-branch 4 (make-branch 7 8))))))

(testing "Testing weight?"
  (is (= false (weight? (right-branch (make-branch 4 (make-branch 7 8)))))))

(testing "Testing weight?"
  (is (= true (weight? (left-branch (make-branch 4 (make-branch 7 8)))))))

(weight? (right-branch (make-branch 6 7)))

(total-weight (make-branch 7 8))
