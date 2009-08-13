(ns old
  (:use clojure.contrib.math))

(defn quadrant-one-pseudo-angle [dx dy]
  (/ dx (+ dy dx)))

(defn pseudo-angle [[dx dy]]
  (cond
    (and (= dx 0) (= dy 0))
    0

    (and (>= dx 0) (> dy 0))
    (quadrant-one-pseudo-angle dx dy)

    (and (> dx 0) (<= dy 0))
    (+ 1 (quadrant-one-pseudo-angle (abs dy) dx))

    (and (<= dx 0) (< dy 0))
    (+ 2 (quadrant-one-pseudo-angle (abs dx) (abs dy)))

    (and (< dx 0) (>= dy 0))
    (+ 3 (quadrant-one-pseudo-angle dy (abs dx)))

    :else nil))

(defn point-min [[x1 y1 :as p1] [x2 y2 :as p2]]
  (cond
    (< x1 x2)
    p1

    (= x1 x2)
    (if (< y1 y2) p1 p2)

    :else
    p2))

(defn find-min-point [points]
  (reduce point-min points))

(defn delta-point [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn angle-and-point [point base]
  [(pseudo-angle (delta-point point base)) point])

(defn min-angle-and-point [ap1 ap2]
  (if (< (first ap1) (first ap2)) ap1 ap2))

(defn find-point-with-least-angle-from [base angle points]
  (reduce min-angle-and-point
    (remove
      #(< (first %) angle)
      (map #(angle-and-point % base)
        (remove
          (fn [p] (= base p))
          points)))))

(defn hull [points]
  (println "Start")
  (let [starting-point (time (find-min-point points))]
    (println starting-point)
    (loop [hull-list [starting-point] angle 0 last-point starting-point]
      (let [[angle next-point] (time (find-point-with-least-angle-from last-point angle points))]
        (if (= next-point (first hull-list))
          hull-list
          (recur (conj hull-list next-point) angle next-point))))))

(def r (java.util.Random.))
(defn rands [] (repeatedly #(.nextGaussian r)))
(defn points [] (take 1000000 (partition 2 (rands))))
(let [hull-points (time (hull (points)))]
  (printf "Points: %d\n" (count hull-points))
  (doseq [x hull-points] (println x)))
