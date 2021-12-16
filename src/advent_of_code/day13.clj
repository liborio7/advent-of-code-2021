(ns advent-of-code.day13
  (:require [advent-of-code.core :refer :all]))

(defn parse-dots-inputs [inputs]
  (->> inputs
       (map (partial re-matches #"(.*),(.*)"))
       (mapcat rest)
       (map #(Integer/parseInt %))
       (partition 2)
       (map (partial zipmap [:x :y]))))

(defn parse-instructions-inputs [inputs]
  (->> inputs
       (map (partial re-matches #"fold along (.*)=(.*)"))
       (map rest)
       (map (partial zipmap [:direction :value]))
       (map (fn [i] (-> i
                        (update :direction keyword)
                        (update :value #(Integer/parseInt %)))))))

(defn parse-input [filename]
  (let [[dots-inputs _ instructions-inputs] (->> (read-file filename)
                                                 (partition-by (partial = "")))]
    {:dots         (parse-dots-inputs dots-inputs)
     :instructions (parse-instructions-inputs instructions-inputs)}))

(defn follow-instruction [{:keys [direction value]} dot]
  (if (>= (direction dot) value)
    (update dot direction (fn [v] (- (* 2 value) v)))
    dot))

(defn solution [instructions-fn filename]
  (let [{:keys [instructions dots]} (parse-input filename)]
    (reduce
      (fn [dots instruction]
        (->> dots
             (map (partial follow-instruction instruction))
             (distinct)))
      dots
      (instructions-fn instructions))))

(defn solution-1 [filename]
  (solution (partial take 1) filename))

(defn print-dots [dots]
  (let [max-x (apply max (map :x dots))
        max-y (apply max (map :y dots))]
    (doseq [y (range (inc max-y))
            x (range (inc max-x))]
      (if (->> dots
               (filter (fn [dot] (and (= (:x dot) x) (= (:y dot) y))))
               ((complement empty?)))
        (print "#")
        (print "."))
      (if (= x max-x)
        (println)))))

(defn solution-2 [filename]
  (let [dots (solution identity filename)]
    (print-dots dots)
    dots))

(comment
  (count (solution-1 "day13.txt"))
  (count (solution-2 "day13.txt"))
  )