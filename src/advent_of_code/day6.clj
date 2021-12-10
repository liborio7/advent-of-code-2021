(ns advent-of-code.day6
  (:require [advent-of-code.core :refer :all]
            [clojure.string :as strings]))

(defn parse-input [filename]
  (->> (read-file filename #(strings/split % #","))
       (first)
       (map #(Integer/parseInt %))
       (frequencies)
       (map (fn [[timer counts]] (hash-map (keyword (str timer)) counts)))
       (into {})
       ))

(defn next-timer [[timer counts]]
  (let [timer     (Integer/parseInt (name timer))
        new-timer (if (zero? timer)
                    6
                    (dec timer))
        new-timer (keyword (str new-timer))]
    (hash-map new-timer counts)))

(defn next-timers [timers]
  (->> timers
       (map next-timer)
       (apply (partial merge-with +))))

(defn play [rounds timers]
  (loop [round  0
         timers timers]
    (if (= round rounds)
      (reduce + (vals timers))
      (let [zeros       (get timers :0 0)
            next-timers (-> (next-timers timers)
                            (update :8 (fnil (partial + zeros) 0)))]
        (recur (inc round)
               next-timers)))))

(defn solution [filename number-of-days]
  (->> (parse-input filename)
       (play number-of-days)))

(defn solution-1 [filename]
  (solution filename 80))

(defn solution-2 [filename]
  (solution filename 256))

(comment
  (time (solution-1 "day6.txt"))
  (time (solution-2 "day6.txt"))
  )
