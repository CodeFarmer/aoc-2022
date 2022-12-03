(ns aoc-2022.core)

(defn lines-as-vector [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (into [] (line-seq rdr))))
