(ns advent-of-code.day1
  (:require [advent-of-code.core :refer :all]))

(defn parse-input []
  (read-file "day1.txt" #(Integer/parseInt %)))

(defn measure-increases [args]
  (->> args
       (partition 2 1)
       (map (partial apply <))
       (filter identity)
       (count)
       ))

(defn sliding-windows [args]
  (->> args
       (partition 3 1)
       (map (partial apply +))
       ))

(defn solution-1 []
  (->> (parse-input)
       (measure-increases)
       ))

(defn solution-2 []
  (->> (parse-input)
       (sliding-windows)
       (measure-increases)
       ))

(comment
  (time (solution-1))
  (time (solution-2))
  )
