(ns aoc-2022.day-9
  (:require [aoc-2022.core :refer :all]))


;; given a pair [x y], return the coordinates if moved one space to the right
(defn move-right [[x y]]
  [(inc x) y])

(defn move-left [[x y]]
  [(dec x) y])

(defn move-up [[x y]]
  [x (inc y)])

(defn move-down [[x y]]
  [x (dec y)])

(defn touching? [[x1 y1] [x2 y2]]
  (and (> 2 (abs (- x1 x2)))
       (> 2 (abs (- y1 y2)))))

(defn move-vertically-towards
  [head tail]
  (let [[head-x head-y] head
        [tail-x tail-y] tail]
    (cond
      (> head-y tail-y) (move-up tail)
      (< head-y tail-y) (move-down tail)
      :default tail)))

(defn move-horizontally-towards
  [head tail]
  (let [[head-x head-y] head
        [tail-x tail-y] tail]
    (cond
      (> head-x tail-x) (move-right tail)
      (< head-x tail-x) (move-left tail)
      :default tail)))

(defn move-towards
  "Given head and tail coordinates, move the tail towards the head if necessary"
  [head tail]
  (if (touching? head tail)
    tail
    (->> tail  
         (move-horizontally-towards head)
         (move-vertically-towards head))))

(defn -move-towards [path-set head tail]
  (let [tail' (move-towards head tail)]
    [(conj path-set tail') tail']))

(defn -move-repeatedly
  "Move the head a number of times using head-fn, and return a tuple containing touched squares (by the tail), head and the tail after it has followed"
  [path-set times move-fn head tail]
  (if (zero? times)
    [path-set head tail]
    (let [head' (move-fn head)
          [path' tail'] (-move-towards path-set head' tail)]
      (recur path' (dec times) move-fn head' tail'))))

(defn move-repeatedly
  "Move the head a number of times using head-fn, and return a pair containing head and the tail after it has followed"
  [times move-fn head tail]
  (let [[_ head' tail']
        (-move-repeatedly #{} times move-fn head tail)]
    [head' tail']))

(def move-commands
  {"R" move-right
   "L" move-left
   "U" move-up
   "D" move-down})

(defn -move-command [path-set move times head tail]
  (-move-repeatedly path-set times (get move-commands move) head tail))

(defn move-command [move times head tail]
  (let [[_ head' tail']
        (-move-command #{} move times head tail)]
    [head' tail']))

(defn -process-commands [path-set move-seq head tail]
  (if (empty? move-seq)
    [path-set head tail]
    (let [[move times] (first move-seq)
          [path' head' tail'] (-move-command path-set move times head tail)]
      (recur path' (rest move-seq) head' tail'))))

(defn process-commands [move-seq head tail]
  (let [[_ head' tail']
        (-process-commands #{} move-seq head tail)]
    [head' tail']))

;; part 2 things

(defn move-rope-to-head [head rope]
  (if (empty? rope) nil
      (let [next-knot (first rope)
            next-knot' (move-towards head next-knot)]
        (cons next-knot' (move-rope-to-head next-knot' (rest rope))))))

(defn -move-rope-repeatedly
  [path-set times move-fn rope]
  (if (zero? times)
    [path-set rope]
    (let [head (first rope)
          head' (move-fn head)
          rope' (cons head' (move-rope-to-head head' (rest rope)))]
      (recur (conj path-set (last rope')) (dec times) move-fn rope'))))

(defn -move-command-rope [path-set move times rope]
  (-move-rope-repeatedly path-set times (get move-commands move) rope))

(defn -process-commands-rope [path-set move-seq rope]
  (if (empty? move-seq)
    [path-set rope]
    (let [[move times] (first move-seq)
          [path' rope'] (-move-command-rope path-set move times rope)]
      (recur path' (rest move-seq) rope'))))

