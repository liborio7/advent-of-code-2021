(ns advent-of-code.day14
  (:require [advent-of-code.core :refer :all]))

(defn parse-polymer-template-input [input]
  (let [input (first input)
        pairs (->> input
                   (partition 2 1)
                   (map (partial apply str))
                   (map keyword))]
    {:pairs       (frequencies pairs)
     :frequencies (frequencies input)}))

(defn parse-pair-insertion-input [input]
  (->> input
       (map (partial re-matches #"(.*) -> (.*)"))
       (mapcat (partial drop 1))
       (map keyword)
       (apply hash-map)))

(defn parse-input [filename]
  (let [[polymer-template-input _ pair-insertion-input] (->> (read-file filename)
                                                             (partition-by (partial = "")))]
    {:template        (parse-polymer-template-input polymer-template-input)
     :pairs-insertion (parse-pair-insertion-input pair-insertion-input)}))

(defn update-template [pairs-insertion template [pair occurrences]]
  (if-let [insertion (get pairs-insertion pair)]
    (let [[first last] (seq (name pair))
          [second] (seq (name insertion))
          first-pair  (keyword (str first second))
          second-pair (keyword (str second last))]
      (-> template
          (update-in [:pairs first-pair] (fnil + 0) occurrences)
          (update-in [:pairs second-pair] (fnil + 0) occurrences)
          (update-in [:frequencies second] (fnil + 0) occurrences)))
    template))

(defn run-polymer-step [{:keys [template pairs-insertion] :as polymer-configuration}]
  (->> (reduce
         (partial update-template pairs-insertion)
         (dissoc template :pairs)
         (:pairs template))
       (assoc polymer-configuration :template)))

(defn solution [filename steps]
  (->> (parse-input filename)
       (iterate run-polymer-step)
       (take (inc steps))
       (last)
       (:template)
       (:frequencies)
       ((juxt (partial apply max-key val) (partial apply min-key val)))
       (map val)
       (reduce -)))

(defn solution-1 [filename]
  (solution filename 10))

(defn solution-2 [filename]
  (solution filename 40))

(comment
  (time (solution-1 "day14.txt"))
  (time (solution-2 "day14.txt"))
  )