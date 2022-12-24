(ns src.two.10-symbolic-differentiation
  (:use [commons :refer :all])
  (:use clojure.test)
  (:use [clojure.math :refer :all]))

(defn variable? [e]
  (symbol? e))

(defn =number? [exp num]
  (and (number? exp) (= exp num)))

(comment
  (let [a 5]
    (testing "Testing variable?"
      (is (= true (variable? 'a))))
    (testing "Testing variable?"
      (is (= false (variable? a))))
    ))

(defn same-variable? [v1 v2]
  (= v1 v2))

(comment
  (testing "Testing same-variable?"
    (is (= true (same-variable? 'a 'a))))
  (testing "Testing same-variable?"
    (is (= false (same-variable? 'a 'b)))))

(defn is-operation? [op e]
  (and (pair? e) (= (car e) op)))

(defn product? [e]
  (is-operation? '* e))

(comment
  (testing "Testing product?"
    (is (= true (product? '(* 4 5)))))

  (testing "Testing product?"
    (is (= false (product? '(not true)))))
  )

(defn multiplier [e]
  (cadr e))

(defn multiplicand [e]
  (caddr e))

(comment
  (testing "Testing multiplier"
    (is (= 4 (multiplier '(* 4 6)))))

  (testing "Testing multiplicand"
    (is (= 5 (multiplicand '(* 4 5)))))
  )

(defn make-product
  [a1 a2]
  (cond
    (or (=number? a1 0) (=number? a2 0)) 0
    (=number? a1 1) a2
    (=number? a2 1) a1
    (and (number? a1) (number? a2)) (* a1 a2)
    (and (number? a2) (variable? a1)) (make-product a2 a1)
    (and (number? a1) (product? a2) (number? (multiplier a2))) (make-product (* a1 (multiplier a2)) (multiplicand a2))
    :else (list '* a1 a2)))

(comment

  (testing "Testing make-product"
    (is (= '(* 4 6) (make-product 4 6))))

  (testing "Testing make-product"
    (is (= '(* 6 a) (make-product 'a 6))))

  (testing "Testing make-product"
    (is (= '(* 6 a) (make-product 6 'a))))
  )

(defn sum? [e]
  (is-operation? '+ e))

(comment
  (testing "Testing sum?"
    (is (= true (sum? '(+ 3 4)))))
  (testing "Testing sum?"
    (is (= false (sum? '(* a 4))))))

(defn addend [e]
  (cadr e))

(comment
  (testing "Testing addend"
    (is (= 3 (addend '(+ 3 4))))))

(defn augend [e]
  (caddr e))

(comment
  (testing "Testing augend"
    (is (= 4 (augend '(+ 3 4)))))
  )


(defn can-be-added? [a1 a2]
  (if-let [[v1 v2]
           (and (product? a1) (product? a2)
                [(multiplicand a1) (multiplicand a2)])]
    (and (variable? v1) (variable? v2) (= v1 v2))
    false))

(defn make-sum [a1 a2]
  (cond
    (=number? a1 0) a2
    (=number? a2 0) a1
    (and (number? a1) (number? a2)) (+ a1 a2)
    (and (variable? a1) (variable? a2) (= a1 a2)) (make-product 2 a1)
    (can-be-added? a1 a2) (make-product (make-sum (multiplier a1) (multiplier a2)) (multiplicand a1))
    :else (list '+ a1 a2)))

(comment
  (testing "Testing make-sum"
    (is (= '(+ 3 c) (make-sum 3 'c))))
  (testing "Testing make-sum"
    (is (= 8 (make-sum 3 5))))
  (testing "Testing make-sum"
    (is (= 'a (make-sum 0 'a))))
  (testing "Testing make-sum"
    (is (= '(* 2 a) (make-sum 'a 'a))))
  (testing "Testing can-be-added?"
    (is (= false (can-be-added? '(+ 5 a) '(+ 7 a)))))
  (testing "Testing can-be-added?"
    (is (= true (can-be-added? '(* 5 a) '(* 7 a)))))
  (testing "Testing make-sum"
    (is (= '(* 12 a) (make-sum '(* 5 a) '(* 7 a)))))
  )


(defn exponentiation? [e]
  (is-operation? '** e))

(comment
  (testing "Testing exponentiation?"
    (is (= true (exponentiation? '(** a b)))))
  (testing "Testing exponentiation?"
    (is (= false (exponentiation? '(* a b)))))
  )

(defn base [e]
  (cadr e))

(defn exponent [e]
  (caddr e))

(comment
  (testing "Testing exponent"
    (is (= 20 (exponent '(** 4 20)))))

  (testing "Testing base"
    (is (= 'a (base '(** a 5)))))
  )

(defn make-exponentiation [b e]
  (cond
    ;; 0^0 = 0 :p
    (=number? b 0) 0
    (=number? e 0) 1
    (and (number? b) (number? e)) (pow b e)
    :else (list '** b e)))

(comment
  (testing "Testing make-exponentiation"
    (is (= '(** 5 a) (make-exponentiation 5 'a))))
  (testing "Testing make-exponentiation"
    (is (= 32.0 (make-exponentiation 2 5))))
  )

(defn derivative [exp var]
  (cond
    (number? exp) 0
    (variable? exp) (if (same-variable? exp var) 1 0)
    (sum? exp) (make-sum
                (derivative (addend exp) var)
                (derivative (augend exp) var))
    (product? exp) (let [u (multiplier exp)
                         v (multiplicand exp)]
                     (make-sum (make-product u (derivative v var))
                               (make-product v (derivative u var))))
    (exponentiation? exp) (let [b (base exp)
                                power (exponent exp)]
                            (make-product (make-product power (make-exponentiation b (- power 1))) (derivative b var)))
    :else (throw (AssertionError. "Unknown expression, can't differentiate"))))

(comment

  (testing "Testing derivative"
    (is (= 1 (derivative '(+ a 5) 'a))))

  (testing "Testing derivative"
    (is (= 0 (derivative 'a 'b))))

  (testing "Testing derivative"
    (is (= '(* 2 a) (derivative '(+ (* a a) 5) 'a))))

  (testing "Testing derivative"
    (is (= '(* 5 (** a 4)) (derivative '(** a 5) 'a))))

  (testing "Testing derivative"
    (is (= '(* 63 (** a 6)) (derivative '(* 9 (** a 7)) 'a))))
  )
