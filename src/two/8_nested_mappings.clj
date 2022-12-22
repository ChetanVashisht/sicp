(ns two.8-nested-mappings
  (:use [commons :refer :all])
  (:use [one.numbertheory :refer [is-prime?]])
  (:use clojure.test)
  (:use [clojure.math :refer :all]))

(comment
  (reduce append nil
          (map (fn [i]
                 (map (fn [j] (list i j))
                      (range 1 (- i 1))))
               (range 1 8))))

(defn flatten-map [proc seq]
  (reduce append nil (map proc seq)))

(defn prime-sum? [pair]
  (is-prime? (+ (car pair) (cadr pair))))

(defn make-pair-sum [pair]
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(defn prime-sum-pairs [n]
  (map make-pair-sum
       (filter prime-sum?
               (flatten-map
                (fn [i] (map (fn [j] (list i j)) (range 1 (- i 1))))
                (range 1 n)))))

(testing "Testing prime-sum-pairs"
  (is (= '((4 1 5) (5 2 7) (6 1 7) (7 4 11)) (prime-sum-pairs 8))))

(defn remove-item [item s]
  (filter #(not= item %) s))

(remove-item 5 (list 1 2 3 5))

(defn permutations [s]
  (if (null? s)
    (list nil)
    (flatmap (fn [x]
               (map (fn [p] (cons x p))
                    (permutations (remove-item x s))))
             s)))

(testing "Testing permutations"
  (is (= '((1 3 5) (1 5 3) (3 1 5) (3 5 1) (5 1 3) (5 3 1)) (permutations (list 1 3 5)))))


(defn subsets [s]
  (if (null? s) (list (list))
      (let [r (subsets (cdr s))]
        (append (map (fn [x] (cons (car s) x)) r) r))))

(testing "Testing subsets"
  (is (= '((1 2 5 6) (1 2 5) (1 2 6) (1 2) (1 5 6) (1 5) (1 6) (1) (2 5 6) (2 5) (2 6) (2) (5 6) (5) (6) ()) (subsets (list 1 2 5 6)))))

;; Ex 2.40
(defn unique-pairs [n]
  (flatten-map
   (fn [x]
     (map
      (fn [y] (list x y))
      (range 1 x)))
   (range 1 n)))

(unique-pairs 5)
;; ↪ ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3))

(defn prime-sum? [pair]
  (is-prime? (+ (car pair) (cadr pair))))

(testing "Testing prime-sum?"
  (is (= true (prime-sum? (list 3 4)))))

(defn prime-sum-pairs [n]
  (->> n
       (unique-pairs)
       (filter prime-sum?)))

(testing "Testing prime-sum-pairs"
  (is (= '((2 1) (3 2) (4 1) (4 3)) (prime-sum-pairs 5))))

;; Ex 2.41
(defn unique-triplets [n]
  (flatmap (fn [x]
             (flatmap (fn [y]
                        (map (fn [z] (list x y z))
                             (range 1 y)))
                      (range 2 x)))
           (range 1 (+ n 1))))

(comment
  (filter #(= (reduce + %) 10) (unique-triplets 5)))

(defn triplet-sum [n s]
  (->> n
       (unique-triplets)
       (filter #(= (reduce + %) s) )))

(testing "Testing triplet-sum"
  (is (= '((5 3 2) (5 4 1) (6 3 1) (7 2 1)) (triplet-sum 10 10))))

;; Ex 2.42 - donesn't work -_-
(defn empty-board [n]
  (map (fn [__] (map (fn [_] 0) (range n))) (range n)))

(empty-board 5)

(defn adjoin-position [new-row k roq]
  (map-indexed (fn [index row]
                 (if (= k index)
                   new-row row)) roq))

(defn safe? [k positions]
  (filter safe-pos positions))

(defn safe-pos [position]
  (let [row-sum (accumulate-n + 0 position)]
    (reduce #(and %1 (or (zero? %2) (= 1 %2))) true row-sum)))

(safe-pos (->> 5
               (empty-board)
               (adjoin-position (list 1 0 0 0 0) 0)
               (adjoin-position (list 0 0 0 1 1) 1)
               (adjoin-position (list 0 0 0 0 1) 2)))

(comment
  (accumulate-n (fn [roq row]
                  (+ roq row)) 0 (->> 5
                                      (empty-board)
                                      (adjoin-position (list 1 0 0 0 0) 0)
                                      (adjoin-position (list 1 0 0 1 0) 1)
                                      (adjoin-position (list 1 0 0 0 1) 2)))

  (->> 5
       (empty-board)
       (adjoin-position (list 1 0 0 0 0) 0)
       (adjoin-position (list 1 0 0 1 0) 1)
       (adjoin-position (list 1 0 0 0 1) 2)))

(comment
  (reduce #(and %1 (or (zero? %2) (= 1 %2))) true (list 1 0 1 2 0)))

(defn queens [board-size]
  (let [queen-cols (fn queen-cols [k]
                     (if (zero? k)
                       #break
                       (empty-board board-size)
                       (filter
                        (fn [positions] (safe? k positions))
                        (flatmap
                         (fn [rest-of-queens]
                           (map
                            (fn [new-row] (adjoin-position new-row k rest-of-queens))
                            (map (fn [k] (map (fn [i] (if (= i k) 1 0)) (range board-size))) (range board-size))))
                         (queen-cols (- k 1))))))]
    (queen-cols board-size)))
