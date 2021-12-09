(ns advent-of-code.day5
  (:require [advent-of-code.core :refer :all]))

(defn parse-vent-input [input]
  (let [matches (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" input)]
    (->> (drop 1 matches)
         (map #(Integer/parseInt %))
         (partition 2))))

(defn parse-input [filename]
  (read-file filename parse-vent-input))

(defn abs-range [start end]
  (if (> end start)
    (range start (inc end))
    (range start (dec end) -1)))

(defn horizontal-vent-points [[[x1 y1] [x2 y2]]]
  (when (= x1 x2)
    (map vector
         (repeat x1)
         (abs-range y1 y2))))

(defn vertical-vent-points [[[x1 y1] [x2 y2]]]
  (when (= y1 y2)
    (map vector
         (abs-range x1 x2)
         (repeat y1))))

(defn abs [n] (max n (- n)))

(defn diagonal-vent-points [[[x1 y1] [x2 y2]]]
  (when (= (abs (- x1 x2)) (abs (- y1 y2)))
    (map vector
         (abs-range x1 x2)
         (abs-range y1 y2))))

(defn solution [filename fs]
  (->> (parse-input filename)
       (mapcat (apply juxt fs))
       (mapcat identity)
       (frequencies)
       (filter (fn [[_ freq]] (>= freq 2)))
       (count)))

(defn solution-1 [filename]
  (solution filename [horizontal-vent-points vertical-vent-points]))

(defn solution-2 [filename]
  (solution filename [horizontal-vent-points vertical-vent-points diagonal-vent-points]))

(comment
  (time (solution-1 "day5.txt"))
  (time (solution-2 "day5.txt"))
  )