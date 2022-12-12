(ns aoc-2022.day-12
  (:require [clojure.string :as str]
            [aoc-2022.core :refer :all]))


(defn elevation [[x y] height-map]
  (let [code (get-in height-map [y x])]
    (case code
      \S (int \a)
      \E (int \z)
      (int code))))


(defn adjacent-points [[x y] height-map]

  (let [width (count (first height-map))
        height (count height-map)]
    (for [dx [-1 0 1]
          dy [-1 0 1]
          :let [x' (+ x dx)
                y' (+ y dy)]
          :when (and (or
                      (and (= dy 0) (not (= dx 0)))
                      (and (= dx 0) (not (= dy 0)))))
          :when (and (>= x' 0) (>= y' 0)
                     (< x' width) (< y' height))]
      
      [x' y'])))


(defn possible-points [[x y] height-map]

  (filter (fn [[x' y']]
            (<= (- (elevation [x' y'] height-map)
                   (elevation [x y] height-map)) 1))
          
   (adjacent-points [x y] height-map)))


(defn find-start [height-map]
  (let [width (count (first height-map))
        height (count height-map)]
    (first (filter (fn [[x y]] (= \S (get-in height-map [y x])))
                   (for [x (range 0 width)
                         y (range 0 height)]
                     [x y])))))


(defn shortest-legal-path
  ([height-map]
   (shortest-legal-path
    (conj (clojure.lang.PersistentQueue/EMPTY)
          (list (find-start height-map)))
    #{}
    height-map))
  
  ([q seen-set height-map]

   (if (empty? q)
     []
     (let [path (peek q)
           [x y] (first path)]

       (if (= \E (get-in height-map [y x]))
         path
         
         (let [unseen-neighbours (filter (complement seen-set)
                                         (possible-points [x y] height-map))]
           (recur (into (pop q) (map #(cons % path) unseen-neighbours))
                  (into seen-set unseen-neighbours)
                  height-map))))
     )))


(defn find-all-lowest-points [height-map]
  (let [width (count (first height-map))
        height (count height-map)]
    (for [x (range 0 width)
          y (range 0 height)
          :when (= \a (get-in height-map [y x]))]
      [x y])))

(defn shortest-from-lowest [height-map]
  ;; the empty? is there becaues it turns out some lowest points can't reach the top at all
  (first
   (drop-while empty?
               (sort-by count
                        (map #(shortest-legal-path
                               (conj (clojure.lang.PersistentQueue/EMPTY) (list %))
                               #{}
                               height-map)
                             (find-all-lowest-points height-map))))))
