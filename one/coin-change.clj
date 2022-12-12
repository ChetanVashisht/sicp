(def values {5 50, 4 25, 3 10, 2 5, 1 1})


(def make-change
  (memoize
   (fn [amt denomination]
     (let [value (get values denomination)]
       (cond
         (= 0 amt) 1
         (or (> 0 amt) (= denomination 0)) 0
         :else (+ (make-change (- amt value) denomination)
                  (make-change amt (- denomination 1))))))))


(defn coin-change [amt]
  (make-change amt 5))

(time (coin-change 100))
