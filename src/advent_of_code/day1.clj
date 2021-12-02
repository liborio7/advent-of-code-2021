(ns advent-of-code.day1
  (:require [advent-of-code.core :refer :all]))

(defn parse-input [input-file]
  (read-file input-file #(Integer/parseInt %)))

(defn measure-increases [args]
  (->> args
       (partition 2 1)
       (filter (partial apply <))
       (count)
       ))

(defn sliding-windows [args]
  (->> args
       (partition 3 1)
       (map (partial apply +))
       ))

(defn solution-1 [input-file]
  (->> (parse-input input-file)
       (measure-increases)
       ))

(defn solution-2 [input-file]
  (->> (parse-input input-file)
       (sliding-windows)
       (measure-increases)
       ))

(comment
  (let [input-file "day1.txt"]
    (time (solution-1 input-file))
    (time (solution-2 input-file))
    ))
