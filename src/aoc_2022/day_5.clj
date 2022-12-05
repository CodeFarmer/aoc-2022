(ns aoc-2022.day-5
  (:require [aoc-2022.core :refer :all]))


;; move the first item from a seq with stack semantics to another
(defn move-1 [from-stack to-stack]
  [(pop from-stack) (conj to-stack (first from-stack))])


;; given a starting state that is a vector of stacks and two 1-indexed stack nimbers, move an item from one stack to another
(defn move-1-in [start-state from-stack-index to-stack-index]
  (let [fsi (dec from-stack-index)
        tsi (dec to-stack-index)
        from-stack (get start-state fsi)
        to-stack (get start-state tsi)
        [from-stack' to-stack'] (move-1 from-stack to-stack)]
    (-> start-state
        (assoc fsi from-stack')
        (assoc tsi to-stack'))))

(defn move-in [start-state num-items from-stack-index to-stack-index]
  start-state
  (if (zero? num-items)
    start-state
    (recur (move-1-in start-state from-stack-index to-stack-index)
           (dec num-items)
           from-stack-index to-stack-index)))

(defn parse-move [move-string]
  (into '[] (intify-seq (re-seq #"\d+" move-string))))

(defn apply-moves [state moves]
  (if (empty? moves)
    state
    (recur (apply (partial move-in state) (first moves))
           (rest moves))))
