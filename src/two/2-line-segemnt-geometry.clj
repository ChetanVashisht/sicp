(ns two.line-segement-geometry
  (:use [commons :refer :all])
  (:use [clojure.math :refer :all]))

;; Ex 2.2

(defn make-point [x y]
  {:x x :y y})

(comment
  (make-point 3 4)
  (:y (make-point 3 4)))

(defn make-segment [start-segment end-segment]
  {:start start-segment :end end-segment})

(let [start-segment (make-point 3 4)
      end-segment (make-point 5 6)]
  (make-segment start-segment end-segment))

(defn midpoint-segment [line-segment]
  (let [start (:start line-segment)
        end (:end line-segment)]
    (make-point (average (:x start) (:x end))
                (average (:y start) (:y end)))))


(let [start-segment (make-point 3 4)
      end-segment (make-point 5 6)]
  (midpoint-segment (make-segment start-segment end-segment)))


;; Ex 2.3

(defn make-rectangle [pt-1 pt-2 pt-3]
  {:side (make-segment pt-1 pt-2)
   :perpendicular-side (make-segment pt-2 pt-3)})

(defn length [segment]
  (let [start (:start segment)
        end (:end segment)]
    (sqrt (+ (square (- (:x start) (:x end)))
             (square (- (:y start) (:y end)))))))

(comment
  (length (make-segement (make-point 0 0) (make-point 3 4))))

(defn area [rect]
  (* (length (:side rect)) (length (:perpendicular-side rect))))

(defn perimeter [rect]
  (* 2 (+ (length (:side rect)) (length (:perpendicular-side rect)))))

(comment
  (let [rect (make-rectangle (make-point 0 0) (make-point 0 10) (make-point -10 10))]
    (print (perimeter rect) (area rect))))

(defn make-rectangle-with-segments [side perpendicular-side]
  {:side side
   :perpendicular-side perpendicular-side})
