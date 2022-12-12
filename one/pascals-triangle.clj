(defn pascal [n m prev]
  (do
    (print prev)
    (if (= n m)
      prev
      (recur n (+ m 1)
             (conj (map-indexed (fn [x _] (+ (nth prev x) (nth prev (+ 1 x) 0))) prev) 1)))))


(comment
  `(~@(map not [true false])))

(pascal 4 1 [1 1])

(comment
  (nth [1 1] 7 0)

  (let [prev [1 1]]
    (conj (map-indexed (fn [x _] (+ (nth prev x) (nth prev (+ 1 x) 0))) prev) 1)))
