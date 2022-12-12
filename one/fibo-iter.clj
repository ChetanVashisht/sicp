(defn fibo [a b n]
  (if (= n 0)
    a
    (fibo (+ a b) a (- n 1))))

(fibo 1 0 10)

;; Exercise 1.11

(defn ex111 [a b c n]
  (cond
    (< n 2) n
    (= n 2) a
    :else (ex111 (+ a b b c c c) a b (- n 1))))

(ex111 2 1 0 4)
(ex111 2 1 0 2)
