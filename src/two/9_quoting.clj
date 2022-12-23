(ns src.two.9-quoting
  (:use [commons :refer :all])
  (:use clojure.test)
  (:use [clojure.math :refer :all]))

;; Ex 2.53
(list 'a 'b 'c)

(list (list 'g))

(cdr '((x1 x2) (y1 y2)))

(cadr '((x1 x2) (y1 y2)))

(pair? (car '(a short list)))

(memq 'red '((red shoes) (blue socks)))

(memq 'red '(a red shoes blue socks))

;; Ex 2.54
(defn equals? [a b]
  (cond
    (and (null? a) (null? b)) true
    (not= (count a) (count b)) false
    :else (and (= (car a) (car b)) (equals? (cdr a) (cdr b)))))

(equals? '(this is a list) '(this is a list))
(equals? '(this is a list) '(this is another list))

;; Ex 2.55
(car ''abcradabra)
