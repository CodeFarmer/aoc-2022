(ns aoc-2022.day-4)

;; given to pairs of numbers representing incdlusive ranges,
;; does the first wholly contain the second?
(defn wholly-contains? [pair-a pair-b]
  (let [[min-a max-a] pair-a
        [min-b max-b] pair-b]
    (and (<= min-a min-b)
         (>= max-a max-b))))

(defn either-contains? [pair-a pair-b]
  (or (wholly-contains? pair-a pair-b)
      (wholly-contains? pair-b pair-a)))

(defn count-overlapping-ranges [pairs-seq]
  (count (filter (partial apply either-contains?) pairs-seq)))
