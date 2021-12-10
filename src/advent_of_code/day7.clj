(ns advent-of-code.day7
  (:require [advent-of-code.core :refer :all]
            [clojure.string :as strings]))

(defn parse-horizontal-positions-input [input]
  (->> (strings/split input #",")
       (map #(Integer/parseInt %))
       ))

(defn parse-input [filename]
  (->> (read-file filename parse-horizontal-positions-input)
       (first)))

(defn abs [n] (max n (- n)))

(defn calculate-fuel [best-position-fn cost-fn positions]
  (let [best-position (best-position-fn positions)]
    (->> (map - positions (repeat best-position))
         (map abs)
         (map cost-fn)
         (reduce +))))

(defn solution [filename best-position-fn cost-fn]
  (->> (parse-input filename)
       (calculate-fuel best-position-fn cost-fn)))

(defn median [ns]
  (let [ns  (sort ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)]
    (if (odd? cnt)
      (nth ns mid)
      (/ (+ (nth ns mid) (nth ns (dec mid))) 2))))

(defn solution-1 [filename]
  (solution filename median identity))

(defn avg-floor [ns]
  (->> (/ (reduce + ns) (count ns))
       (float)
       (Math/floor)
       (int)))

(defn solution-2 [filename]
  (solution filename avg-floor #(reduce + (range (inc %)))))

(comment
  (time (solution-1 "day7.txt"))
  (time (solution-2 "day7.txt"))
  )
