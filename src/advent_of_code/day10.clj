(ns advent-of-code.day10
  (:require [advent-of-code.core :refer :all]))

(defn parse-input [filename]
  (read-file filename))

(def open-close
  {\( \)
   \{ \}
   \[ \]
   \< \>})

(def corrupted-character-score
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(defn open? [character]
  ((set (keys open-close)) character))

(defn close? [stack character]
  (->> stack
       (:expected-close)
       (first)
       (= character)))

(defn check-syntax [characters]
  (letfn [(reducing-fn [stack character]
            (cond
              (open? character) (update stack :expected-close (partial cons (get open-close character)))
              (close? stack character) (update stack :expected-close rest)
              :else (reduced (assoc stack :first-corrupted-character character))))]
    (reduce reducing-fn {} characters)))

(defn corrupted-score [stack]
  (if-let [corrupted-character (:first-corrupted-character stack)]
    (corrupted-character-score corrupted-character)
    0))

(defn solution-1 [filename]
  (->> (parse-input filename)
       (map check-syntax)
       (map corrupted-score)
       (reduce +)))

(def close-score
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn autocomplete-score [stack]
  (let [expected-close (:expected-close stack)]
    (->> expected-close
         (map close-score)
         (reduce (fn [total score] (+ score (* 5 total))) 0))))

(defn median [ns]
  (let [ns  (sort ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)]
    (if (odd? cnt)
      (nth ns mid)
      (/ (+ (nth ns mid) (nth ns (dec mid))) 2))))

(defn solution-2 [filename]
  (->> (parse-input filename)
       (map check-syntax)
       (remove :first-corrupted-character)
       (map autocomplete-score)
       (median)))

(comment
  (time (solution-1 "day10.txt"))
  (time (solution-2 "day10.txt"))
  )