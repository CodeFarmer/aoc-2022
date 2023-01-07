(ns aoc-2022.day-14
  (:require [clojure.string :as str]
            [aoc-2022.core :refer :all]))

(defn parse-path [line]
  (map #(intify-seq (str/split % #","))
       (str/split line #" -> "))) 

(defprotocol Cave
  (bounds [self])
  (rock-path [self point-seq])
  (to-string [self])
  (drop-sand [self grain])
  (drop-sand-infinite-floor [self grain]))


(defn -set-cave-bounds
  ([cave]
   (-set-cave-bounds [Integer/MAX_VALUE Integer/MAX_VALUE]
                [Integer/MIN_VALUE Integer/MIN_VALUE]
                cave))
  ([[minx miny] [maxx maxy] cave]
   (if (empty? cave)
     [[minx miny] [maxx maxy]]
     (let [[x y] (first cave)
           minx' (min minx x)
           miny' (min miny y)
           maxx' (max maxx x)
           maxy' (max maxy y)]
       (recur [minx' miny'] [maxx' maxy'] (rest cave))))))

(defn -set-cave-bounds-with-sand
  [cave sand]
  (-set-cave-bounds (into cave sand)))

(defn -set-line-between [[x1 y1] [x2 y2]]
  (into #{}
        (for [x (if (> x2 x1 )
                  (range x1 (inc x2))
                  (range x2 (inc x1)))
              y (if (> y2 y1)
                  (range y1 (inc y2))
                  (range y2 (inc y1)))]
          [x y])))

(defn -set-cave-tick
  [cave sand [grain-x grain-y]]
  (let [straight-down [grain-x (inc grain-y)]
        down-left [(dec grain-x) (inc grain-y)]
        down-right [(inc grain-x) (inc grain-y)]]
    (cond (not (or (cave straight-down)
                   (sand straight-down)))
          straight-down
          (not (or (cave down-left)
                   (sand down-left)))
          down-left
          (not (or (cave down-right)
                   (sand down-right)))
          down-right
          :default [grain-x grain-y])))


(defrecord SetCave [stone-set sand-set]
  
  Cave
  
  (bounds [self]
    (-set-cave-bounds-with-sand stone-set sand-set))
  
  (rock-path [self path]
    (SetCave. (reduce into stone-set
                      (map (partial apply -set-line-between) (partition 2 1 path)))
              sand-set))  
  
  (to-string [self]
    (str/join "\n"
              (map (partial apply str)
                   (let [[[minx miny] [maxx maxy]] (bounds self)
                         width (inc (- maxx minx))
                         height (inc (- maxy miny))]
                     (reduce (fn [acc [x y c]]
                               (assoc-in acc [(- y miny) (- x minx)] c))
                             (apply vector (repeat height []))
                             (for [x (range minx (inc maxx))
                                   y (range miny (inc maxy))
                                   :let [c (if (stone-set [x y])
                                             \#
                                             (if (sand-set [x y])
                                               \o
                                               \.))]]
                               [x y c]))))))

  (drop-sand [self grain]
    (let [[[minx miny] [maxx maxy]] (-set-cave-bounds stone-set)
          [x' y'] (-set-cave-tick stone-set sand-set grain)]
      (cond (= grain [x' y']) ;; grain has stopped
            (SetCave. stone-set (conj sand-set grain))
            (or (< x' minx)
                (> x' maxx)
                (> y' maxy)) ;; fallen off the map
            self
            :default (recur [x' y']))))

  (drop-sand-infinite-floor [self grain]
    (let [[[minx miny] [maxx maxy]] (-set-cave-bounds stone-set)
          [x' y'] (-set-cave-tick stone-set sand-set grain)]
      (cond (= grain [x' y']) ;; grain has stopped
            (SetCave. stone-set (conj sand-set grain))
            (> y' (inc maxy)) ;; hit the floor
            (SetCave. stone-set (conj sand-set grain))
            :default (recur [x' y'])))))

(defn set-cave []
  (SetCave. #{} #{}))


(defn all-rock-paths [cave paths]
  (reduce rock-path cave paths))


;; part 2

(defn -drop-a-lot-of-sand
  ([cave drop-fn starting-point]
   (iterate (fn [cave] (drop-fn cave starting-point))
            cave)))

(defn drop-a-lot-of-sand
  ([cave n starting-point]
   (drop-a-lot-of-sand cave drop-sand n starting-point))
  ([cave drop-fn n starting-point]
   (nth (-drop-a-lot-of-sand cave drop-fn starting-point) n)))

(defn last-caught-sand
  ([cave starting-point]
   (last-caught-sand cave drop-sand starting-point))
  ([cave drop-fn starting-point]
   (loop [index 0
          state-pairs (partition 2 1 (-drop-a-lot-of-sand cave drop-fn starting-point))]
     (comment 
       (if (= 0 (mod index 100))
         (println (to-string (first (first state-pairs))) "\n" index "\n")))
     (if (apply = (first state-pairs))
       index
       (recur (inc index) (rest state-pairs))))))
