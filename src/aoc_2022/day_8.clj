(ns aoc-2022.day-8
  (:require [aoc-2022.core :refer :all]))

;; This problem seemed nice, but the repetition is horrible and
;; while you can make it go away to some extent, the juice is not
;; worth the squeeze. Blargh.

;; I guess you could build a visibility map up front and just traverse
;; each row and column once in each direction, for linear time instead
;; of quadratic

(defn ctoi [achar]
  (- (int achar) 48))

(defn tree-height [forest x y]
  (get-in forest [y x]))

(defn visible-from-top? [forest x y]
  (let [th (tree-height forest x y)]
    (not (some #(>= (tree-height forest x %) th)
               (range 0 y)))))

(defn visible-from-bottom? [forest x y]
  (let [th (tree-height forest x y)
        height (count forest)]
      (not (some #(>= (tree-height forest x %) th)
                 (range (inc y) height)))))

(defn visible-from-left? [forest x y]
  (let [th (tree-height forest x y)]
    (not (some #(>= (tree-height forest % y) th)
               (range 0 x)))))

(defn visible-from-right? [forest x y]
  (let [th (tree-height forest x y)
        width (count (first forest))]
    (not (some #(>= (tree-height forest % y) th)
               (range (inc x) width)))))

(defn visible? [forest x y]
          (or (visible-from-top? forest x y)
              (visible-from-bottom? forest x y)
              (visible-from-left? forest x y)
              (visible-from-right? forest x y)))

(defn count-visible-trees [forest]
  (let [width (count (first forest))
        height (count forest)]
    (count (filter true? (for [x (range 0 width)
                               y (range 0 height)]
                           (visible? forest x y))))))

;; an alernative solution, more code but linear-ish instead of quadratic
;; other directions left as an exercise for the reader

(defn visibility-from-left-row-map
  ([row]
   (visibility-from-left-row-map 0 [] row))
  ([max-height acc row]
   (if (empty? row)
     acc
     (if (< max-height (first row))
       (recur (first row) (conj acc true) (rest row))
       (recur max-height (conj acc false) (rest row))))))

(defn visibility-from-left-forest-map [forest]
  (into [] (map visibility-from-left-row-map forest)))

(defn visible-from-left-mapped? [left-visibility-map x y]
  (get-in left-visibility-map [y x]))

;; scenic things, part 2

(defn take-until
  "Take items from aseq until pred returns true, including the item that triggered pred"
  [pred aseq]
  (lazy-seq
   (when-let [s (seq aseq)]
     (if (pred (first s))
       (cons (first s) nil)
       (cons (first s) (take-until pred (rest aseq)))))))
  

(defn viewing-distance-up [forest x y]
  (let [th (tree-height forest x y)]
    (count (take-until #(<= th %)
                       (for [y' (range (dec y) -1 -1)]
                         (tree-height forest x y'))))))

(defn viewing-distance-down [forest x y]
  (let [th (tree-height forest x y)
        height (count forest)]
    (count (take-until #(<= th %)
                       (for [y' (range (inc y) height)]
                         (tree-height forest x y'))))))

(defn viewing-distance-left [forest x y]
  (let [th (tree-height forest x y)]
    (count (take-until #(<= th %)
                       (for [x' (range (dec x) -1 -1)]
                         (tree-height forest x' y))))))

(defn viewing-distance-right [forest x y]
  (let [th (tree-height forest x y)
        width (count forest)]
    (count (take-until #(<= th %)
                       (for [x' (range (inc x) width)]
                         (tree-height forest x' y))))))

(defn scenic-score [forest x y]
  (* (viewing-distance-left forest x y)
     (viewing-distance-right forest x y)
     (viewing-distance-up forest x y)
     (viewing-distance-down forest x y)))

(defn highest-scenic-score [forest]
  (apply max (for [x (range 0 (count (first forest)))
                   y (range 0 (count forest))]
               (scenic-score forest x y))))
