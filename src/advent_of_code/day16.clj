(ns advent-of-code.day16
  (:require [advent-of-code.core :refer :all]
            [clojure.string :as strings]
            [clojure.walk :refer [postwalk]]))

(defn hex->binary [char]
  (case char
    \0 "0000"
    \1 "0001"
    \2 "0010"
    \3 "0011"
    \4 "0100"
    \5 "0101"
    \6 "0110"
    \7 "0111"
    \8 "1000"
    \9 "1001"
    \A "1010"
    \B "1011"
    \C "1100"
    \D "1101"
    \E "1110"
    \F "1111"))

(defn binary->long [str]
  (Long/parseLong str 2))

(defn parse-input [filename]
  (let [hex-input    (first (read-file filename))
        binary-input (->> hex-input
                          (map hex->binary)
                          (reduce str))]
    {:hexadecimal hex-input
     :binary      binary-input}))

(defn take-until
  ([pred coll]
   (lazy-seq
     (when-let [s (seq coll)]
       (if-not (pred (first s))
         (cons (first s) (take-until pred (rest s)))
         (list (first s)))))))

(def version-length 3)
(def type-length 3)
(def headers-length (+ version-length type-length))
(def literal-content-length 5)
(def operator-type-length 1)
(def operator-0-type-length 15)
(def operator-1-type-length 11)

(declare decode)

(defn decode-literal-packet [binary-str]
  (let [binary-str     (subs binary-str headers-length)
        content        (->> binary-str
                            (partition literal-content-length)
                            (map (partial apply str))
                            (take-until #(strings/starts-with? % "0")))
        content-length (reduce + (map count content))
        number         (->> content
                            (map #(subs % 1))
                            (reduce str)
                            (binary->long))]
    {:length  (+ headers-length content-length)
     :literal number}))

(defn decode-operator-0-packet [binary-str]
  (let [binary-str         (subs binary-str (+ headers-length operator-type-length))
        sub-packets-length (->> (subs binary-str 0 operator-0-type-length)
                                (binary->long))
        sub-packets        (decode (fn [_ packets]
                                     (-> (reduce + (map :length packets))
                                         (>= sub-packets-length)))
                                   identity
                                   (subs binary-str operator-0-type-length))]
    {:length      (+ headers-length
                     operator-type-length
                     operator-0-type-length
                     sub-packets-length)
     :sub-packets sub-packets}))

(defn decode-operator-1-packet [binary-str]
  (let [binary-str         (subs binary-str (+ headers-length operator-type-length))
        sub-packets-count  (->> (subs binary-str 0 operator-1-type-length)
                                (binary->long))
        sub-packets        (decode (fn [_ packets]
                                     (-> (count packets)
                                         (>= sub-packets-count)))
                                   identity
                                   (subs binary-str operator-1-type-length))
        sub-packets-length (reduce + (map :length sub-packets))]
    {:length      (+ headers-length
                     operator-type-length
                     operator-1-type-length
                     sub-packets-length)
     :sub-packets sub-packets}))

(defn decode-operator-packet [operator binary-str]
  (let [type    (subs binary-str headers-length (+ headers-length operator-type-length))
        content (case type
                  "0" (decode-operator-0-packet binary-str)
                  "1" (decode-operator-1-packet binary-str))]
    (assoc content :operator operator)))

(defn decode-next-packet [binary-str]
  (let [version (binary->long (subs binary-str 0 version-length))
        type    (binary->long (subs binary-str version-length (+ version-length type-length)))
        content (case type
                  0 (decode-operator-packet + binary-str)
                  1 (decode-operator-packet * binary-str)
                  2 (decode-operator-packet min binary-str)
                  3 (decode-operator-packet max binary-str)
                  4 (decode-literal-packet binary-str)
                  5 (decode-operator-packet > binary-str)
                  6 (decode-operator-packet < binary-str)
                  7 (decode-operator-packet = binary-str))]
    (assoc content :type type :version version)))

(defn mod-floor [div num]
  (if (zero? (mod num div))
    num
    (+ num (- div (mod num div)))))

(defn decode
  ([binary-str]
   (decode
     (fn [binary-str _packets] (empty? binary-str))
     (fn [packet] (update packet :length (partial mod-floor 8)))
     binary-str))
  ([until-pred adjust-packet-fn binary-str]
   (loop [binary-str binary-str
          packets    []]
     (if (until-pred binary-str packets)
       packets
       (let [packet (decode-next-packet binary-str)
             packet (adjust-packet-fn packet)]
         (recur (subs binary-str (:length packet))
                (conj packets packet)))))))

(defn solution [evaluate-fn filename]
  (->> (parse-input filename)
       (:binary)
       (decode)
       (first)
       (evaluate-fn)))

(defn solution-1 [filename]
  (letfn [(sum-version [packet]
            (->> packet
                 (tree-seq :sub-packets :sub-packets)
                 (map :version)
                 (reduce +)))]
    (solution sum-version filename)))

(defn boolean->int [bool]
  (when (boolean? bool)
    (if bool 1 0)))

(defn solution-2 [filename]
  (letfn [(evaluate-expr [packet]
            (postwalk
              (fn [node]
                (let [operator    (:operator node)
                      sub-packets (:sub-packets node)
                      literal     (:literal node)]
                  (cond
                    operator (->> sub-packets
                                  (map (fn [v] (or (boolean->int v) v)))
                                  (apply operator))
                    literal literal
                    :default node)))
              packet))]
    (solution evaluate-expr filename)))

(comment
  (time (solution-1 "day16.txt"))
  (time (solution-2 "day16.txt"))
  )