(ns advent-of-code.day12
  (:require [advent-of-code.core :refer :all]
            [clojure.string :as strings]))

(defn parse-connection-input [input]
  (let [[c1 c2] (strings/split input #"-")]
    [{:from c1 :to c2}
     {:from c2 :to c1}]))

(defn parse-input [filename]
  (flatten (read-file filename parse-connection-input)))

(defn start-cave? [cave]
  (= "start" cave))

(defn end-cave? [cave]
  (= "end" cave))

(defn small-cave? [cave]
  (and
    (not (start-cave? cave))
    (not (end-cave? cave))
    (re-matches #"[a-z]+" cave)))

(defn visit
  ([forbidden-caves-pred connections] (visit forbidden-caves-pred connections "start"))
  ([forbidden-caves-pred connections from] (visit forbidden-caves-pred connections from {}))
  ([forbidden-caves-pred connections from visited]
   (let [visited         (update visited from (fnil inc 0))
         forbidden-caves (forbidden-caves-pred visited)]
     (if (end-cave? from)
       visited
       (->> connections
            (filter (fn [connection] (= from (:from connection))))
            (map :to)
            (remove (set forbidden-caves))
            (map #(visit forbidden-caves-pred connections % visited))
            (flatten))))))

(defn solution [visitable-pred filename]
  (->> (parse-input filename)
       (visit visitable-pred)
       (count)))

(defn filter-small-or-start-caves [visited]
  (->> (keys visited)
       (filter (fn [cave] (or (small-cave? cave) (start-cave? cave))))))

(defn solution-1 [filename]
  (->> (parse-input filename)
       (visit filter-small-or-start-caves)
       (count)))

(defn can-visit-twice? [visited]
  (->> visited
       (filter (comp small-cave? key))
       (filter (comp (partial < 1) val))
       (empty?)))

(defn solution-2 [filename]
  (->> (parse-input filename)
       (visit (fn [visited]
                (if (can-visit-twice? visited)
                  ["start"]
                  (filter-small-or-start-caves visited))))
       (count)))

(comment
  (time (solution-1 "day12.txt"))
  (time (solution-2 "day12.txt"))
  )
