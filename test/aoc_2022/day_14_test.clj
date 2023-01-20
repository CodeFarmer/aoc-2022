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

(deftest set-line-between-test
  (testing "a vertical line between the first two points is the set of all points between them inclusive"
    (is (= #{[498 4] [498 5] [498 6]}
           (-set-line-between [498 4] [498 6]))))
  (testing "a vertical line between the second two points"
    (is (= #{[496 6] [497 6] [498 6]}
           (-set-line-between [498 6] [496 6]))))
  (testing "a line between two points is the same whichever way the points are ordered"
    (is (= (-set-line-between [498 6] [496 6])
           (-set-line-between [496 6] [498 6])))))

(def toy-paths (map parse-path toy-lines))

(deftest multi-segment-line-test
  (testing "a path on a single line of input, drawn on an empty cave, returns the expected rock squares"
    (is (= #{[497 6] [498 5] [496 6] [498 6] [498 4]}
           (:stone-set (rock-path (set-cave) (first toy-paths)))))))

(def toy-set-cave (all-rock-paths (set-cave) toy-paths))
(println "\ntoy-set-cave\n")
(println (to-string toy-set-cave))

(def toy-vec-cave (all-rock-paths (vector-cave 510 510) toy-paths))
(println "\ntoy-vec-cave\n")
(println (to-string toy-vec-cave))

(deftest multi-multi-segment-test
  (is (= "....#...##
....#...#.
..###...#.
........#.
........#.
#########."
         (to-string toy-vec-cave))))

(deftest moving-sand-test
  (testing "sand will drop directly down one unit if it can"
    (is (= [500 2]
           (tick (set-cave) [500 1]))))
  (testing "sand will drop diagonally down-left one unit if it can't go straight down"
    (is (= [499 2]
           (tick (->SetCave #{[500 2]} #{}) [500 1]))))
  (testing "sand will drop diagonally down-right one unit if it can't go straight down or down-left"
    (is (= [501 2]
           (tick (->SetCave #{[500 2] [499 2]} #{}) [500 1]))))
  (testing "a grain which can do none of these things will stay where it is"
    (is (= [500 1]
           (tick (->SetCave #{[499 2] [500 2] [501 2]} #{}) [500 1])))))

(def dropping-sand-test
  (testing "dropping a single grain of sand into a cave works correctly"
    (is (= "....#...##
....#...#.
..###...#.
........#.
......o.#.
#########."
           (to-string (drop-sand toy-set-cave [500 1]))))))

(deftest dropping-lots-of-sand-test
  (testing "Dropping consecutive grains of sand builds up as expected"
    (is (= "......o...
.....ooo..
....#ooo##
....#ooo#.
..###ooo#.
....oooo#.
...ooooo#.
#########."
           (to-string (drop-a-lot-of-sand toy-set-cave 22 [500 1]))))))

(deftest steady-state-detection-test
  (testing "After 24 drops, no more sand accumulates"
    (is (= 24 (last-caught-sand toy-set-cave [500 1]))))
  (testing "After 93 drops, no more sand accumulates when we assume an infinite floor 2 units below the lowest stone"
    (is (= 93 (last-caught-sand toy-set-cave drop-sand-infinite-floor [500 0])))))


;; problems

(def test-cave (all-rock-paths (set-cave) (map parse-path (lines-as-vector "input-14.txt"))))

(deftest part-1-test
  (is (= 805 (last-caught-sand test-cave [500 1]))))

;; FIXME this currently takes several minutes to run and I suspect
;; it's the set membership operations getting really expensive for
;; vectors when n is really big
;;
;; refactor to not use sets, just fill a big grid and look for nearby
;; obstacles

(comment
  (deftest part-2-test
    (is (= 25161 (last-caught-sand test-cave drop-sand-infinite-floor [500 0])))))
