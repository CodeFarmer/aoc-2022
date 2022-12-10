(ns aoc-2022.day-10
  (:require [clojure.string :as str]
            [aoc-2022.core :refer :all]))


(defn noop [[clock x]]
  [(inc clock) x])

(defn addx [[clock x] arg]
  [(+ 2 clock) (+ x arg)])

(defn operation-line [state line]
  (let [line-tokens (str/split line #" ")]
    (case (first line-tokens)
      "noop" (noop state)
      "addx" (addx state (Integer/parseInt (second line-tokens))))))

(defn machine-state-sequence [state lines]
  (lazy-seq
   (if (empty? lines)
     '()
     (let [state' (operation-line state (first lines))]
       (comment (println (first lines) state'))
       (cons state'
             (machine-state-sequence state' (rest lines)))))))

(defn state-at-time [time state-seq]
  (last (take-while (fn [[t x]] (< t time)) state-seq)))

(defn signal-strengths-at-times
  [times state-seq]
  (if (empty? times) '()
      (let [time (first times)
            [t x] (state-at-time time state-seq)]
        (cons (* time x) (signal-strengths-at-times (rest times) state-seq)))))

(defn pixels-visible-at-time [time state-seq]
  (let [[t x] (state-at-time time state-seq)]
    #{(dec x) x (inc x)}))

(defn pixel-state-at-time [time state-seq]
  (if ((pixels-visible-at-time time state-seq) (mod (dec time) 40))
    "#"
    "."))
