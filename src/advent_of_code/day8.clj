(ns advent-of-code.day8
  (:require [advent-of-code.core :refer :all]
            [clojure.string :as strings]
            [clojure.set :as sets]))

(defn parse-digits [input]
  (->> (strings/split input #" | ")
       (map set)
       (partition-by (partial = #{\|}))
       (partition 3)
       (map #(hash-map :in (first %) :out (last %)))
       (into {})))

(defn parse-input [filename]
  (read-file filename parse-digits))

(defn solution-1 [filename]
  (->> (parse-input filename)
       (mapcat :out)
       (map count)
       (filter (partial #{2 3 4 7}))
       (count)))

(defn decode-in [in-digits]
  (let [digits-by-count (group-by count in-digits)
        conf-1          (first (get digits-by-count 2))
        conf-7          (first (get digits-by-count 3))
        conf-4          (first (get digits-by-count 4))
        conf-8          (first (get digits-by-count 7))
        conf-6          (->> (get digits-by-count 6)
                             (remove (partial sets/subset? conf-1))
                             (first))
        conf-0          (->> (get digits-by-count 6)
                             (remove #{conf-6})
                             (remove (partial sets/subset? conf-4))
                             (first))
        conf-9          (->> (get digits-by-count 6)
                             (remove #{conf-0 conf-6})
                             (first))
        conf-3          (->> (get digits-by-count 5)
                             (filter (partial sets/subset? conf-1))
                             (first))
        conf-2          (->> (get digits-by-count 5)
                             (filter #(= 2 (count (sets/difference conf-4 %))))
                             (first))
        conf-5          (->> (get digits-by-count 5)
                             (remove #{conf-3 conf-2})
                             (first))
        ]
    {conf-0 "0" conf-1 "1" conf-2 "2" conf-3 "3" conf-4 "4"
     conf-5 "5" conf-6 "6" conf-7 "7" conf-8 "8" conf-9 "9"}))

(defn decode-out [in-conf out-digits]
  (map (partial get in-conf) out-digits))

(defn decode [digits]
  (let [in-conf (decode-in (:in digits))]
    (->> (:out digits)
         (decode-out in-conf)
         (apply str)
         (Integer/parseInt))))

(defn solution-2 [filename]
  (->> (parse-input filename)
       (map decode)
       (reduce +)))

(comment
  (time (solution-1 "day8.txt"))
  (time (solution-2 "day8.txt"))
  )