(ns advent-of-code.day4
  (:require [advent-of-code.core :refer :all]
            [clojure.string :as strings]))

(defn parse-string-numbers [pred string]
  (->> (strings/split string pred)
       (map #(Integer/parseInt %))))

(defn parse-drawn-numbers-input [input]
  (parse-string-numbers #"\," input))

(defn parse-board-input [input]
  (->> input
       (map strings/trim)
       (map (partial parse-string-numbers #"\s+"))))

(defn parse-boards-inputs [inputs]
  (->> inputs
       (partition-by #{""})
       (drop 1)
       (take-nth 2)
       (map parse-board-input)))

(defn parse-input [filename]
  (let [content (read-file filename)]
    {:drawn-numbers (parse-drawn-numbers-input (first content))
     :boards        (parse-boards-inputs (rest content))}))

(defn mark-board [number board]
  (let [mark-row (partial map (fn [x] (if (= x number) -1 x)))]
    (map mark-row board)))

(defn rotate-board [board]
  (apply map vector board))

(defn has-marked-row? [board]
  (->> board
       (filter (fn [row] (= -1 (reduce max row))))
       ((complement empty?))))

(defn has-marked-col? [board]
  (has-marked-row? (rotate-board board)))

(defn bingo? [marked-board]
  (->> marked-board
       ((juxt has-marked-row? has-marked-col?))
       (reduce #(or %1 %2))))

(defn calculate-final-score [marked-board number]
  (->> marked-board
       (flatten)
       (remove #{-1})
       (reduce +)
       (* number)))

(defn play-bingo [drawn-numbers boards]
  (loop [round         0
         marked-boards boards]
    (let [number            (nth drawn-numbers round)
          new-marked-boards (map (partial mark-board number) marked-boards)
          bingo-boards      (filter bingo? new-marked-boards)]
      (if (not (empty? bingo-boards))
        (calculate-final-score (first bingo-boards) number)
        (recur (inc round)
               new-marked-boards)))))

(defn play-giant-squid [drawn-numbers boards]
  (loop [round       0
         boards-left boards]
    (let [number        (nth drawn-numbers round)
          marked-boards (map (partial mark-board number) boards-left)
          bingo-boards  (filter bingo? marked-boards)]
      (if (and (= 1 (count marked-boards)) (not (empty? bingo-boards)))
        (calculate-final-score (first bingo-boards) number)
        (recur (inc round)
               (remove bingo? marked-boards))))))

(defn solution-1 [filename]
  (let [{:keys [drawn-numbers boards]} (parse-input filename)]
    (play-bingo drawn-numbers boards)))

(defn solution-2 [filename]
  (let [{:keys [drawn-numbers boards]} (parse-input filename)]
    (play-giant-squid drawn-numbers boards)))

(comment
  (time (solution-1 "day4.txt"))
  (time (solution-2 "day4.txt"))
  )
