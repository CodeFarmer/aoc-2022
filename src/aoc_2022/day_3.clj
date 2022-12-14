(ns aoc-2022.day-3
  (:require [clojure.set :as set]))


(defn split-rucksack
  "Split a string into two character sequences representing its equally-sized compartments"
  [rucksack-string]
  (split-at (/ (count rucksack-string) 2) rucksack-string))

;; given a seq of seqables, find the common item between all of them
(defn common-item [aseq]
  (first (apply set/intersection (map #(into #{} %) aseq))))

(defn rucksack-common-item [rucksack-string]
  (common-item (split-rucksack rucksack-string)))


(defn item-priority [item-char]
  (let [item-code (int item-char)]
    (if (> item-code 90)
      (- item-code 96)
      (- item-code 38))))

(defn rucksack-common-item-priority [rucksack-string]
  (item-priority (rucksack-common-item rucksack-string)))

;; (def test-data
;;   (lines-as-vector "input-3.txt"))

;; part 1
;; (reduce + (map rucksack-common-item-priority test-data))

;; part 2
;; (reduce + (map item-priority (map common-item (partition 3 test-data))))
