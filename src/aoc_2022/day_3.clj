(ns aoc-2022.day-3
  (:require [clojure.set :as set]))


(defn split-rucksack
  "Split a string into two character sequences representing its equally-sized compartments"
  [rucksack-string]
  (split-at (/ (count rucksack-string) 2) rucksack-string))

(defn common-item [a b]
  (first (set/intersection (into #{} a) (into #{} b))))

(defn rucksack-common-item [rucksack-string]
  (apply common-item (split-rucksack rucksack-string)))


(defn item-priority [item-char]
  (let [item-code (int item-char)]
    (if (> item-code 90)
      (- item-code 96)
      (- item-code 38))))

(defn rucksack-common-item-priority [rucksack-string]
  (item-priority (rucksack-common-item rucksack-string)))
