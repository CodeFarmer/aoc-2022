(ns aoc-2022.day-6
  (:require [aoc-2022.core :refer :all]))

;; This is not actually good for sliding windows, but Moore's Law and small test data for the win
(defn has-repeats?
  ([aseq]
   (has-repeats? #{} aseq))
  ([seen-set aseq]
   (if (empty? aseq)
     false
     (if (seen-set (first aseq))
       true
       (recur (conj seen-set (first aseq))
              (rest aseq))))))

;; is this efficient? of course not. Is it easy to understand? I guess?
(defn non-repeating-string-index
  "Find the first index after which there are len non-repeating characters in a row"
  ([len astr]
   (non-repeating-string-index 0 len astr))
  ([index len astr]
   (if (has-repeats? (take len astr))
     (non-repeating-string-index (inc index) len (rest astr))
     (+ len index))))
