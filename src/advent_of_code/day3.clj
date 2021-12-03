(ns advent-of-code.day3
  (:require [advent-of-code.core :refer :all]))

(defn parse-input [filename]
  (read-file filename seq))

(defn rotate-report-matrix [report-matrix]
  (apply map vector report-matrix))

(def rotate-report-matrix-memo
  (memoize rotate-report-matrix))

(defn reduce-bit-seq-frequencies [reduce-fn default-value bit-seq]
  (let [bit-seq-frequencies (frequencies bit-seq)]
    (if (reduce = (map second bit-seq-frequencies))
      default-value
      (->> bit-seq-frequencies
           (apply reduce-fn second)
           (first)))))

(defn most-common-bit [bit-seq]
  (reduce-bit-seq-frequencies max-key \1 bit-seq))
(defn less-common-bit [bit-seq]
  (reduce-bit-seq-frequencies min-key \0 bit-seq))

(defn rate-fn [mask-fn]
  (fn [matrix]
    (->> matrix
         (rotate-report-matrix-memo)
         (map mask-fn))))

(defn match-mask-fn [mask idx]
  (fn [bit-seq]
    (->> [bit-seq mask]
         (map #(nth % idx))
         (reduce =))))

(defn rating-fn [mask-fn]
  (fn [report-matrix]
    (loop [round  0
           matrix report-matrix]
      (if (= 1 (count matrix))
        (first matrix)
        (let [mask ((rate-fn mask-fn) matrix)]
          (recur (inc round)
                 (filter (match-mask-fn mask round) matrix)))))))

(defn binary-seq-to-decimal [bit-seq]
  (Integer/parseInt (apply str bit-seq) 2))

(defn solution [mask-fn filename]
  (->> (parse-input filename)
       ((juxt (mask-fn most-common-bit) (mask-fn less-common-bit)))
       (map binary-seq-to-decimal)
       (reduce *)))

(defn solution-1 [filename]
  (solution rate-fn filename))

(defn solution-2 [filename]
  (solution rating-fn filename))

(comment
  (time (solution-1 "day3.txt"))
  (time (solution-2 "day3.txt"))
  )