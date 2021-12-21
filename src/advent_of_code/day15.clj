(ns advent-of-code.day15
  (:require [advent-of-code.core :refer :all]
            [clojure.data.priority-map :refer [priority-map priority-map-keyfn]]))

(defn parse-risk-inputs [inputs]
  (mapv #(Integer/parseInt (str %)) inputs))

(defn parse-input [repeat-fn filename]
  (repeat-fn (read-file filename parse-risk-inputs)))

(defn idx-keyword [row col]
  (keyword (str row "-" col)))

(defn get-risk [risk-map [row col]]
  (let [risk (-> risk-map (get row) (get col))]
    [(idx-keyword row col) risk]))

(defn get-adjacent [m [row col]]
  (->> [[(dec row) col]
        [row (dec col)] [row (inc col)]
        [(inc row) col]]
       (map (partial get-risk m))
       (filter second)
       (into {})))

(defn build-risk-graph [risk-matrix]
  (let [rows (count risk-matrix)
        cols (count (first risk-matrix))]
    (into {} (for [row (range rows)
                   col (range cols)
                   :let [adjacent (get-adjacent risk-matrix [row col])]]
               {(idx-keyword row col)
                adjacent}))))

(defn update-costs [graph costs unvisited curr]
  (let [curr-cost (get costs curr)]
    (reduce-kv
      (fn [acc-costs node node-cost]
        (if (unvisited node)
          (update acc-costs node min (+ curr-cost node-cost))
          acc-costs))
      costs
      (get graph curr))))


(defn dijkstra [graph src dst]
  (loop [costs     (-> (apply priority-map (interleave (keys graph) (repeat Long/MAX_VALUE)))
                       (assoc src 0))
         curr      src
         unvisited (-> (apply hash-set (keys graph))
                       (disj src))]
    (cond
      (= curr dst)
      (-> (select-keys costs [dst])
          (first)
          (val))

      (or (zero? (count unvisited)) (= Long/MAX_VALUE (get costs curr)))
      costs

      :else
      (let [next-costs     (update-costs graph costs unvisited curr)
            next-node      (->> (filter #(unvisited (key %)) next-costs)
                                (first)
                                (key))
            next-unvisited (disj unvisited next-node)]
        (recur next-costs
               next-node
               next-unvisited)))))

(defn solution [repeat-fn filename]
  (let [input   (parse-input repeat-fn filename)
        max-row (dec (count input))
        max-col (dec (count (first input)))]
    (-> input
        (build-risk-graph)
        (dijkstra (idx-keyword 0 0) (idx-keyword max-row max-col))
        )))

(defn expand-matrix [times matrix]
  (letfn [(sum [n1 n2] (let [mod (mod (+ n1 n2) 9)]
                         (if (zero? mod) 9 mod)))]
    (->> matrix
         (map (fn [m]
                (map
                  (partial mapv sum)
                  (repeat times m)
                  (map (partial repeat (count matrix)) (range times)))))
         (map flatten)
         (repeat)
         (map (fn [value m]
                (map (partial mapv (partial sum value)) m))
              (range times))
         (reduce into []))))

(defn solution-1 [filename]
  (solution identity filename))

(defn solution-2 [filename]
  (solution (partial expand-matrix 5) filename))

(comment
  (time (solution-1 "day15-demo.txt"))
  (time (solution-2 "day15.txt"))
  )