(ns aoc-2022.day-11
  (:require [clojure.string :as str]
            [aoc-2022.core :refer :all]))

;; I promised myself I was going to do the timple thing for day 11. I lied.
;; eg ((apply partial (:operation m)) 2)
(defn make-op [op-string arg-string]
  (list (resolve (symbol op-string)) (Integer/parseInt arg-string)))

;; I hate parsing text so much
(defn parse-monkey [monkey-string]
  {:number (Integer/parseInt (second (re-find #"Monkey (\d)" monkey-string)))
   :items (intify-seq (str/split (second (re-find #"Starting items: (.*)\n" monkey-string)) #", "))
   :operation (apply make-op (rest (re-find #"Operation: new = old ([\+\*]) (\d+)" monkey-string)))
   :test-divisible (Integer/parseInt (second (re-find #"divisible by (\d+)" monkey-string)))
   :true-monkey (Integer/parseInt (second (re-find #"If true: throw to monkey (\d+)" monkey-string)))
   :false-monkey (Integer/parseInt (second (re-find #"If false: throw to monkey (\d+)" monkey-string)))})
