(ns scopes)

(def x 2)

(let [x 3
      y (+ x 2)]
  (* x y))

(defn f [g]
  (g 2))

(f #(* % %))

(f f)
