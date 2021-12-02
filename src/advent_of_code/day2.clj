(ns advent-of-code.day2
  (:require [advent-of-code.core :refer :all]
            [clojure.string :as strings]))

(defn parse-input-line [input-line]
  (let [[direction unit] (strings/split input-line #" ")]
    {:direction (keyword direction)
     :unit      (Integer/parseInt unit)}))

(defn parse-input [input-file]
  (read-file input-file parse-input-line))

(defn update-submarine [k f unit submarine]
  (update submarine k #(f % unit)))

(defmulti instruction :direction)
(defmethod instruction :forward [{:keys [unit]}]
  (partial update-submarine :horizontal-position + unit))
(defmethod instruction :down [{:keys [unit]}]
  (partial update-submarine :depth + unit))
(defmethod instruction :up [{:keys [unit]}]
  (partial update-submarine :depth - unit))

(defmulti instruction-with-aim :direction)
(defmethod instruction-with-aim :forward [{:keys [unit]}]
  (fn [submarine]
    (->> submarine
         (update-submarine :horizontal-position + unit)
         (update-submarine :depth + (* unit (:aim submarine)))
         )))
(defmethod instruction-with-aim :down [{:keys [unit]}]
  (partial update-submarine :aim + unit))
(defmethod instruction-with-aim :up [{:keys [unit]}]
  (partial update-submarine :aim - unit))

(defn solution [input-file instruction-fn]
  (let [instructions (->> (parse-input input-file)
                          (map instruction-fn)
                          (reverse)
                          (reduce comp))]
    (->> {:horizontal-position 0 :depth 0 :aim 0}
         (instructions)
         ((juxt :horizontal-position :depth))
         (reduce *)
         )))

(defn solution-1 [input-file]
  (solution input-file instruction))

(defn solution-2 [input-file]
  (solution input-file instruction-with-aim))

(comment
  (let [input-file "day2-demo.txt"]
    (time (solution-1 input-file))
    (time (solution-2 input-file))
    ))