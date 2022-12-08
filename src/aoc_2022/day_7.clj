(ns aoc-2022.day-7
  (:require [aoc-2022.core :refer :all]
            [clojure.string :as str]))


(defn split-on-spaces [astr]
  #(str/split astr #" "))
