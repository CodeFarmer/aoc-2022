(ns aoc-2022.day-9-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [aoc-2022.core :refer :all]
            [aoc-2022.day-9 :refer :all]))

(deftest movement-test
  (testing "ends can be moved one space at a time"
    (is (= [1 0] (move-right [0 0])))
    (is (= [-1 0] (move-left [0 0])))
    (is (= [0 1] (move-up [0 0])))
    (is (= [0 -1] (move-down [0 0])))))

(deftest touching-test
  (testing "two ends are touching if they are vertically, horizontally or diagonally adjacent or in the same place"
    (is (touching? [1 1] [2 1]))
    (is (touching? [2 3] [3 2]))
    (is (touching? [2 3] [3 2]))
    (is (touching? [1 1] [1 1])))
  (testing "two ends are not touching if they are vertically or horizontally separated by one or more spaces"
    (is (not (touching? [1 1] [3 1])))
    (is (not (touching? [1 3] [1 1])))
    (is (not (touching? [1 1] [2 3])))
    (is (not (touching? [1 1] [3 2])))))

(deftest move-towards-test
  (testing "a non-separated tail will not move toward the head"
    (is (= [1 1] (move-towards [2 1] [1 1]))))
  (testing "a horizontally separated tail will move horizontally toward the head"
    (is (= [2 1] (move-towards [1 1] [3 1])))
    (is (= [2 1] (move-towards [3 1] [1 1]))))
  (testing "a vertically separated tail will move vertically toward the head"
    (is (= [1 2] (move-towards [1 1] [1 3])))
    (is (= [1 2] (move-towards [1 3] [1 1]))))
  (testing "a diagonally separated tail will move diagonally toward the head"
    (is (= [2 2] (move-towards [2 3] [1 1])))
    (is (= [2 2] (move-towards [3 2] [1 1])))))

(deftest move-head-test
  (testing "Moving the head repeatedly causes the tail to follow it"
    (is (= [[4 0] [3 0]] (move-repeatedly 4 move-right [0 0] [0 0])))
    (is (= [[4 4] [4 3]] (move-repeatedly 4 move-up    [4 0] [3 0])))
    (is (= [[1 4] [2 4]] (move-repeatedly 3 move-left  [4 4] [4 3])))
    (is (= [[1 3] [2 4]] (move-repeatedly 1 move-down  [1 4] [2 4]))))
  (testing "The head can be moved to cover the tail"
    (is (= [[2 1] [2 1] (move-repeatedly 1 move-right [1 1] [2 1])]))))

(def toy-data
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(defn parse-line [line]
  (let [[command num & _] (str/split line #" ")]
    [command (Integer/parseInt num)]))

(def toy-moves
  (map parse-line (str/split toy-data #"\n")))

(deftest process-commands-test
  (testing "Sequences of commands are processed correctly"
    (is (= [[2 2] [1 2]] (process-commands toy-moves [0 0] [0 0])))))

;; this is going to be very annoying

(deftest path-counting-test
  (testing "Sequences of commands correctly track the places the tail has been"
    (is (= 13
           (count (first (-process-commands #{} toy-moves [0 0] [0 0])))))))

(def more-toy-data "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(def more-toy-moves
  (map parse-line (str/split more-toy-data #"\n")))

;; I got tired at this point and winged it

(defn rope-of-length [n]
  (into [] (repeat n [0 0])))

(deftest multi-knot-rope-test
  (testing "A rope traces out the correct length"
    (is (= 36 (count (first (-process-commands-rope #{} more-toy-moves (rope-of-length 10))))))))

;; Problems

(def test-moves (map parse-line (lines-as-vector "input-9.txt")))

(deftest part-1-test
  (testing "The right answer for part 1 is calculated"
    (is (= 6503 (count (first (-process-commands #{} test-moves [0 0] [0 0])))))))

(deftest part-2-test
  (testing "The right answer to part 2 is calculated"
    (is (= 2724 (count (first (-process-commands-rope #{} test-moves (rope-of-length 10))))))))
