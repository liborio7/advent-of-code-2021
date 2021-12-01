(ns advent-of-code.core)

(defn read-file
  ([filename] (read-file filename identity))
  ([filename line-coercion]
   (with-open [reader (->> filename
                           (clojure.java.io/resource)
                           (clojure.java.io/reader))]
     (->> reader
          (line-seq)
          (map line-coercion)
          (into [])))))

