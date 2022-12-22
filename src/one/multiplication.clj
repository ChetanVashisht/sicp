(defn doub [a]
  (+ a a))

(defn half [a]
  (/ a 2))

(defn minus1 [a]
  (- a 1))

(defn multiply [a b]
  (cond
    (= 1 b) a
    (even? b) (multiply (doub a) (half b))
    :else (+ a (multiply a (- b 1)))))

(multiply 4 7)

(defn mult [ans a b]
  (cond
    (= b 0) 0
    (= b 1) (+ ans a)
    (even? b) (mult ans (doub a) (half b))
    :else (mult (+ ans a) a (minus1 b))))


(mult 0 4 15)
