(ns advent-of-code.day11
  (:require [advent-of-code.core :refer :all]))

(defn parse-octopus-input [input]
  (->> input
       (map #(Integer/parseInt (str %)))
       (apply vector)))

(defn parse-input [filename]
  (read-file filename parse-octopus-input))

(defn get-octopus [octopuses [row col]]
  (-> octopuses
      (get row)
      (get col)))

(defn octopuses-positions-to-flash [octopuses]
  (let [rows (count octopuses)
        cols (count (first octopuses))]
    (for [row (range rows)
          col (range cols)
          :let [pos     [row col]
                octopus (get-octopus octopuses pos)]
          :when (> octopus 9)]
      pos)))

(defn get-neighbours-positions [octopuses [row col]]
  (let [rows (count octopuses)
        cols (count (first octopuses))]
    (for [row (range (dec row) (+ 2 row))
          col (range (dec col) (+ 2 col))
          :when (and (not (neg? row)) (not (neg? col))
                     (< row rows) (< col cols))]
      [row col])))

(defn inc-octopus-position [octopuses position]
  (let [octopus (get-octopus octopuses position)]
    (cond
      (= 0 octopus) octopuses
      :else (update-in octopuses position inc))))

(defn flash [octopuses]
  (loop [octopuses (mapv (partial mapv inc) octopuses)
         flashes   0]
    (let [positions-to-flash (octopuses-positions-to-flash octopuses)
          octopuses          (reduce #(assoc-in %1 %2 0) octopuses positions-to-flash)]
      (if (empty? positions-to-flash)
        {:octopuses octopuses
         :flashes   flashes}
        (recur (->> positions-to-flash
                    (mapcat (partial get-neighbours-positions octopuses))
                    (reduce inc-octopus-position octopuses))
               (+ flashes (count positions-to-flash)))))))

(defn play-until [pred octopuses]
  (loop [round         0
         octopuses     octopuses
         total-flashes 0]
    (if (pred round octopuses)
      {:round     round
       :octopuses octopuses
       :flashes   total-flashes}
      (let [{:keys [octopuses flashes]} (flash octopuses)]
        (recur (inc round)
               octopuses
               (+ total-flashes flashes))))))

(defn solution-1 [filename]
  (->> (parse-input filename)
       (play-until (fn [round _] (= round 100)))
       (:flashes)
       ))

(defn solution-2 [filename]
  (->> (parse-input filename)
       (play-until (fn [_ octopuses] (every? (partial every? zero?) octopuses)))
       (:round)))

(comment
  (time (solution-1 "day11.txt"))
  (time (solution-2 "day11.txt"))
  )
