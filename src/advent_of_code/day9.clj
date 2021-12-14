(ns advent-of-code.day9
  (:require [advent-of-code.core :refer :all]))

(defn parse-heights-input [input]
  (->> input
       (map str)
       (map #(Integer/parseInt %))
       (apply vector)))

(defn parse-input [filename]
  (read-file filename parse-heights-input))

(defn get-height [heightmap [row col]]
  (let [height (-> heightmap
                   (get row)
                   (get col))]
    {:pos [row col] :value height}))

(defn get-adjacent [heightmap [row col]]
  (->> [[(dec row) col] [(inc row) col] [row (dec col)] [row (inc col)]]
       (map (partial get-height heightmap))
       (filter (comp identity :value))))

(defn lowest-height? [heightmap pos]
  (let [height   (get-height heightmap pos)
        adjacent (get-adjacent heightmap pos)]
    (->> (map :value adjacent)
         (map (partial < (:value height)))
         (every? identity))))

(defn lowest-heights [heightmap]
  (let [heightmap-rows (count heightmap)
        heightmap-cols (count (first heightmap))]
    (for [row (range heightmap-rows)
          col (range heightmap-cols)
          :let [pos [row col]]
          :when (lowest-height? heightmap pos)]
      (get-height heightmap pos))))

(defn solution-1 [filename]
  (let [heightmap (parse-input filename)]
    (->> (lowest-heights heightmap)
         (map :value)
         (map inc)
         (reduce +))))

(defn basin [heightmap {:keys [pos value]}]
  (let [downward-adjacent (->> (get-adjacent heightmap pos)
                               (filter (fn [height] (< value (:value height))))
                               (remove (fn [height] (= 9 (:value height)))))]
    (into (set downward-adjacent) (mapcat (partial basin heightmap) downward-adjacent))))

(defn solution-2 [filename]
  (let [heightmap (parse-input filename)]
    (->> (lowest-heights heightmap)
         (map (partial basin heightmap))
         (map count)
         (sort)
         (take-last 3)
         (map inc)
         (reduce *))))

(comment
  (time (solution-1 "day9.txt"))
  (time (solution-2 "day9.txt"))
  )