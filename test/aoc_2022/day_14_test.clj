(ns aoc-2022.day-14-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [aoc-2022.core :refer :all]
            [aoc-2022.day-14 :refer :all]))


(def toy-lines (str/split "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"
                         #"\n"))

(deftest parse-path-test
  (testing "path is parsed correctly to sequences of points"
    (is (= [[498 4] [498 6] [496 6]]
           (parse-path (first toy-lines))))))

(deftest line-between-test
  (testing "a vertical line between the first two points is the set of all points between them inclusive"
    (is (= [[498 4] [498 5] [498 6]]
           (line-between [498 4] [498 6]))))
  (testing "a vertical line between the second two points"
    (is (= [[496 6] [497 6] [498 6]]
           (line-between [498 6] [496 6]))))
  (testing "a line between two points is the same whichever way the points are ordered"
    (is (= (line-between [498 6] [496 6])
           (line-between [496 6] [498 6])))))

(def toy-paths (map parse-path toy-lines))

(deftest multi-segment-line-test
  (testing "a path on a single line of input, drawn on an empty cave, returns the expected rock squares"
    (is (= #{[497 6] [498 5] [496 6] [498 6] [498 4]}
           (rock-path #{} (first toy-paths))))))

(def toy-cave (all-rock-paths #{} toy-paths))

(deftest multi-multi-segment-test
  (is (= "....#...##
....#...#.
..###...#.
........#.
........#.
#########."
         (ctos toy-cave))))

(deftest moving-sand-test
  (testing "sand will drop directly down one unit if it can"
    (is (= [500 2]
           (tick toy-cave [500 1]))))
  (testing "sand will drop diagonally down-left one unit if it can't go straight down"
    (is (= [499 2]
           (tick #{[500 2]} [500 1]))))
  (testing "sand will drop diagonally down-right one unit if it can't go straight down or down-left"
    (is (= [501 2]
           (tick #{[500 2] [499 2]} [500 1]))))
  (testing "a grain which can do none of these things will stay where it is"
    (is (= [500 1]
           (tick #{[499 2] [500 2] [501 2]} [500 1])))))

(def dropping-sand-test
  (testing "dropping a single grain of sand into a cave works correctly"
    (is (= "....#...##
....#...#.
..###...#.
........#.
......o.#.
#########."
           (apply ctos (drop-sand toy-cave #{} [500 1]))))))

(deftest dropping-lots-of-sand-test
  (testing "Dropping consecutive grains of sand builds up as expected"(is (= "......o...
.....ooo..
....#ooo##
....#ooo#.
..###ooo#.
....oooo#.
...ooooo#.
#########."
                                                                             (apply ctos (drop-a-lot-of-sand toy-cave #{} 22 [500 1]))))))

(deftest steady-state-detection-test
  (testing "After 24 drops, no more sand accumulates"
    (is (= 24 (last-caught-sand toy-cave #{} [500 1]))))
  (testing "After 93 drops, no more sand accumulates when we assume an infinite floor 2 units below the lowest stone"
    (is (= 93 (last-caught-sand toy-cave #{} drop-sand-infinite-floor [500 0])))))


;; problems

(def test-cave (all-rock-paths #{} (map parse-path (lines-as-vector "input-14.txt"))))

(deftest part-1-test
  (is (= 805
         (last-caught-sand test-cave #{} [500 1]))))

(deftest part-2-test
  (is (= 25161 (last-caught-sand test-cave #{} drop-sand-infinite-floor [500 0]))))
