(ns hull (:use clojure.contrib.math))

(defmacro quadrant-one-pseudo-angle [dx dy]
  `(/ ~dx (+ ~dy ~dx)))

(defn point-min [p1 p2]
  (let [x1 (first p1)
	y1 (second p1)
	x2 (first p2)
	y2 (second p2)]
    (cond
     (< x1 x2)
     p1

     (= x1 x2)
     (if (< y1 y2) p1 p2)
     :else
     p2)))

(defn find-angle [base-point next-point angle]
  (let [new-angle  (let  [dx (- (first next-point) (first base-point))
			  dy (- (second next-point) (second base-point))]
		     (cond
		      (and (== dx 0) (== dy 0)), 0

		      (and (>= dx 0) (> dy 0)), (quadrant-one-pseudo-angle dx dy)

		      (and (> dx 0) (<= dy 0)), (+ 1 (quadrant-one-pseudo-angle (abs dy) dx))

		      (and (<= dx 0) (< dy 0)), (+ 2 
						   (quadrant-one-pseudo-angle 
						    (abs dx) (abs dy)))

		      (and (< dx 0) (>= dy 0)), (+ 3 (quadrant-one-pseudo-angle dy (abs dx)))
		      :else 4))]

    (if (< new-angle angle) 4 new-angle)))

(defn find-min-angle-point [base-point pts angle]
  (let [points (remove #(= base-point %) pts)]
    (loop [best-point (first points)
	   best-angle (find-angle base-point (first points) angle)
	   next-point (second points)
	   tail (rest (rest points))]

      (let [next-angle (find-angle base-point next-point angle)
	    comparison (< best-angle next-angle)
	    winner-point (if comparison best-point next-point)
	    winner-angle (if comparison best-angle next-angle)]

	(if (next tail)
	  (recur winner-point winner-angle (first tail) (rest tail))
	  [winner-point winner-angle])))))


(defn hull [points]
  (let [start-point (reduce point-min points)]
    (loop [hull-points [start-point] current-point start-point angle 0]
      (let [[found-point angle] (find-min-angle-point current-point points angle)]
	(if (= found-point start-point)
	  hull-points
	  (recur (conj hull-points found-point) found-point angle))))))


(let [r (java.util.Random.)
	rands (fn [] (repeatedly #(.nextGaussian r)))
	points (fn [] (vec (take 1000000 (partition 2 (rands)))))]
    (let [pts (points)
	  hull-points (time (hull pts))]
      (printf "Points: %d\n" (count hull-points))
      (doseq [x hull-points] (println x))))