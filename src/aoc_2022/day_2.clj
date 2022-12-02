(ns aoc-2022.day-2)


(defn shape-score [shape]
  (case shape
    :rock 1
    :paper 2
    :scissors 3
    0))


(def your-shape-codes
  {"A" :rock
   "B" :paper
   "C" :scissors})

(def my-shape-codes
  {"X" :rock
   "Y" :paper
   "Z" :scissors})

;; shape [defeats loses-to]
(def shape-rules
  {:rock [:scissors :paper]
   :scissors [:paper :rock]
   :paper [:rock :scissors]})


(defn result-score [your-shape my-shape]
  (let [[win-shape lose-shape] (get shape-rules my-shape)]
    (cond
      (= win-shape your-shape) 6
      (= lose-shape your-shape) 0
      (= your-shape my-shape) 3)))


(defn round-score [your-shape my-shape]
  (+ (shape-score my-shape)
     (result-score your-shape my-shape)))


(defn round-score-predetermined [your-shape-enc my-shape-enc]
  (let [my-shape (get my-shape-codes my-shape-enc)]
    (round-score (get your-shape-codes your-shape-enc)
                 my-shape)))


(defn round-score-deliberate [your-shape-enc my-outcome-enc]
  (let [your-shape (get your-shape-codes your-shape-enc)
        [lose-shape win-shape] (get shape-rules your-shape)
        my-shape (case my-outcome-enc
                   "X" lose-shape
                   "Y" your-shape
                   "Z" win-shape)]    
    (round-score your-shape my-shape)))

