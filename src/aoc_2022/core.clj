(ns aoc-2022.core)

(defn split-on-empty-lines
  "Given a seq of strings, split into subseqs using the empty string as a separator"
  ([aseq] (split-on-empty-lines [] aseq))
  ([acc aseq]
   (if (empty? aseq)
     acc
     (let [[group remainder] (split-with (complement empty?) aseq)]
       (recur (conj acc group) (rest remainder))))))

(defn intify
  "Transform a sequence of strings into a sequence of integers"
  [aseq]
  (map #(Integer/parseInt %) aseq))

(defn sums [aseq]
  "Transform a sequence of sequences of integers, into a sequence of their sums"
  (map #(reduce + %) aseq))

;; (def test-data
;;   (with-open [rdr (clojure.java.io/reader "input-1.txt")]
;;     (into [] (line-seq rdr))
;;   ))

;; answer to question 1
;; (apply max (sums (map intify (split-on-empty-lines test-data))))

;; answer to question 2
;; (apply + (take 3 (sort > (sums (map intify (split-on-empty-lines test-data))))))
