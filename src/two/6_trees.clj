(ns two.6-trees
  (:use [commons :refer :all])
  (:use clojure.test)
  (:use [clojure.math :refer :all]))

(defn count-leaves [x]
  (cond
    (null? x) 0
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
        (append (if (pair? d)
                  (fringe d)
                  (list d))
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
  (cdr mobile))

(defn weight? [structure]
  (not (pair? structure)))

(defn weight [branch]
  branch)

(defn total-weight [mobile]
  (+ (cond
       (null? mobile) 0
       (weight? mobile) (weight mobile)
       :else (+ (total-weight (left-branch mobile))
                (total-weight (right-branch mobile))))))

(testing "Testing right-branch"
  (is (= '((7 8)) (right-branch (make-branch 4 (make-branch 7 8))))))

(testing "Testing weight?"
  (is (= false (weight? (right-branch (make-branch 4 (make-branch 7 8)))))))

(testing "Testing weight?"
  (is (= true (weight? (left-branch (make-branch 4 (make-branch 7 8)))))))

(testing "Testing total-weight"
  (is (= 15 (total-weight (make-mobile 7 8)))))

(testing "Testing total-weight"
  (is (= 105 (total-weight (make-branch 5 (make-branch 5 (make-mobile (make-mobile 20 45) 30)))))))


;; Scale tree and mapping functions
(defn scale-tree [tree factor]
  (cond
    (null? tree) nil
    (not (pair? tree)) (* tree factor)
    :else (cons (scale-tree (car tree) factor)
                (scale-tree (rest tree) factor))))

(testing "Testing scale-tree"
  (is (= '(5 (10 (15 20) 25) (30 35)) (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 5))))

(defn scale-tree [tree factor]
  (map (fn [subtree]
         (if (pair? subtree)
           (scale-tree subtree factor)
           (* subtree factor)))
       tree))

(testing "Testing scale-tree"
  (is (= '(5 (10 (15 20) 25) (30 35)) (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 5))))

(scale-tree (list 1 2 3) 5)

;; Ex 2.30

(defn square-tree [tree]
  (cond
    (null? tree) nil
    (not (pair? tree)) (square tree)
    :else (cons (square-tree (car tree))
                (square-tree (cdr tree)))))

(testing "Testing square-tree"
  (is (= '(1 (4 (9 16) 25) (36 49)) (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))))

(defn square-tree [tree]
  (map (fn [subtree]
         (if (pair? subtree)
           (square-tree subtree)
           (square subtree)))
       tree))

(testing "Testing square-tree"
  (is (= '(1 (4 (9 16) 25) (36 49)) (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))))

;; Ex 2.31
(defn tree-map [f tree & args]
  (cond
    (null? tree) nil
    (not (pair? tree)) (apply f tree args)
    :else (cons (apply tree-map f (car tree) args)
                (apply tree-map f (cdr tree) args))))

(defn square-tree [tree] (tree-map square tree))

(testing "Testing square-tree"
  (is (= '(1 (4 (9 16) 25) (36 49)) (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))))

(defn scale-tree [tree factor] (tree-map * tree factor))

(testing "Testing scale-tree"
  (is (= '(10 (20 (30 40) 50) (60 70)) (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10))))

;; Ex 2.32
(defn subsets [s]
  (if (null? s) '(())
      (let [r (subsets (cdr s))]
        (append r (map (fn [x](cons (car s) x)) r)))))

(testing "Testing subsets"
  (is (= '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)) (subsets (list 1 2 3)))))
