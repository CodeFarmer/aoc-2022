(ns aoc-2022.day-14
  (:require [clojure.string :as str]
            [aoc-2022.core :refer :all]))


(defn parse-path [line]
  (map #(intify-seq (str/split % #","))
       (str/split line #" -> "))) 


(defn -cave-bounds
  ([cave]
   (-cave-bounds [Integer/MAX_VALUE Integer/MAX_VALUE]
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

(def cave-bounds
  (memoize -cave-bounds))

(defn cave-bounds-with-sand
  [cave sand]
  (cave-bounds (into cave sand)))

(defn line-between [[x1 y1] [x2 y2]]
  (for [x (if (> x2 x1 )
            (range x1 (inc x2))
            (range x2 (inc x1)))
        y (if (> y2 y1)
            (range y1 (inc y2))
            (range y2 (inc y1)))]
    [x y]))

(defn rock-path [cave path]
  (reduce into cave (map (partial apply line-between) (partition 2 1 path))))

(defn ctos
  "cave to string. this is the product of a tired mind"
  ([cave]
   (ctos cave #{}))
  ([cave sand]
   (str/join "\n"
             (map (partial apply str)
                  (let [[[minx miny] [maxx maxy]] (cave-bounds-with-sand cave sand)
                        width (inc (- maxx minx))
                        height (inc (- maxy miny))]
                    (reduce (fn [acc [x y c]]
                              (assoc-in acc [(- y miny) (- x minx)] c))
                            (apply vector (repeat height []))
                            (for [x (range minx (inc maxx))
                                  y (range miny (inc maxy))
                                  :let [c (if (cave [x y])
                                            \#
                                            (if (sand [x y])
                                              \o
                                              \.))]]
                              [x y c])))))))

(defn all-rock-paths [cave paths]
  (reduce rock-path cave paths))

(defn tick
  ([cave grain]
   (tick cave #{} grain))
  ([cave sand [grain-x grain-y]]
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
           :default [grain-x grain-y]))))


(defn -drop-sand [cave sand grain]
  (let [[[minx miny] [maxx maxy]] (cave-bounds cave)
        [x' y'] (tick cave sand grain)]
    (cond (= grain [x' y']) ;; grain has stopped
          [cave (conj sand grain)]
          (or (< x' minx)
              (> x' maxx)
              (> y' maxy)) ;; fallen off the map
          [cave sand]
          :default (recur cave sand [x' y']))))

(defn drop-sand [cave sand grain]
  (-drop-sand cave sand grain))

;; part 2

(defn drop-sand-infinite-floor [cave sand grain]
  (let [[[minx miny] [maxx maxy]] (cave-bounds cave)
        [x' y'] (tick cave sand grain)]
    (cond (= grain [x' y']) ;; grain has stopped
          [cave (conj sand grain)]
          (> y' (inc maxy)) ;; hit the floor
          [cave (conj sand grain)]
          :default (recur cave sand [x' y']))))

(defn -drop-a-lot-of-sand
  ([cave sand drop-fn starting-point]
   (iterate (fn [[cave sand]] (drop-fn cave sand starting-point))
            [cave sand])))

(defn drop-a-lot-of-sand
  ([cave sand n starting-point]
   (drop-a-lot-of-sand cave sand drop-sand n starting-point))
  ([cave sand drop-fn n starting-point]
   (nth (-drop-a-lot-of-sand cave sand drop-fn starting-point) n)))

(defn -last-caught-sand
  ([cave sand starting-point]
   (-last-caught-sand cave sand drop-sand starting-point))
  ([cave sand drop-fn starting-point]
   (loop [index 0
          state-pairs (partition 2 1 (-drop-a-lot-of-sand cave sand drop-fn starting-point))]
     (comment (if (= 0 (mod index 100))
                (println (apply ctos (first (first state-pairs))) "\n" index "\n")))
     (if (apply = (first state-pairs))
       index
       (recur (inc index) (rest state-pairs))))))

(defn last-caught-sand
  ([cave sand starting-point]
   (-last-caught-sand cave sand starting-point))
  ([cave sand drop-fn starting-point]
   (-last-caught-sand cave sand drop-fn starting-point)))
