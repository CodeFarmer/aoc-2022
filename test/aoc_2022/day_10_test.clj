(ns aoc-2022.day-10-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [aoc-2022.core :refer :all]
            [aoc-2022.day-10 :refer :all]))

(def toy-data
  "noop
addx 3
addx -5")

(def toy-lines (str/split toy-data #"\n"))

(deftest noop-test
  (testing "noop takes 1 cycle and does not modify X"
    (is (= [1 1] (noop [0 1])))))

(deftest addx-test
  (testing "addx takes 2 cycles and updates X with its argument"
    (is (= [3 4] (addx [1 1] 3)))
    (is [= [5 -1] (addx [3 4] -5)])))

(deftest operation-line-test
  (testing "Operation lines are parsed and performed correctly"
    (is (= [1 1] (operation-line [0 1] "noop")))
    (is (= [3 4] (operation-line [1 1] "addx 3")))))

(deftest operation-lines-test
  (testing "Operation lines are performed in sequence"
    (is (= [5 -1] (reduce operation-line [0 1] toy-lines)))))

(deftest machine-state-sequence-test
  (testing "the state of a machine can be modeled as a sequence"
    (is (= [[1 1] [3 4] [5 -1]] (machine-state-sequence [0 1] toy-lines)))))

(def bigger-toy-data (lines-as-vector "input-10-small.txt"))

(deftest state-at-time-test

  (testing "The state of a machine can be queried at a time that is the middle of an operation"
    (is (= [1 1] (state-at-time 2 (machine-state-sequence [0 1] toy-lines)))))
  
  (testing "The state of a machine can be queried arbitrarily according to the example"
    (let [state-seq (machine-state-sequence [0 1] bigger-toy-data)]
      (is (= 21 (second (state-at-time 20 state-seq))))
      (is (= 19 (second (state-at-time 60 state-seq))))
      (is (= 18 (second (state-at-time 100 state-seq))))
      (is (= 21 (second (state-at-time 140 state-seq))))
      (is (= 16 (second (state-at-time 180 state-seq))))
      (is (= 18 (second (state-at-time 220 state-seq)))))))

(deftest signal-strengths-times-test
  (testing "Given a series of times and a machine state sequence, signal strengths can be mapped"
    (let [state-seq (machine-state-sequence [0 1] bigger-toy-data)]
      (is (= [420 1140 1800 2940 2880 3960]
             (signal-strengths-at-times [20 60 100 140 180 220] state-seq))))))

;; problems

(def test-data (lines-as-vector "input-10.txt"))
(def test-state-seq (cons [0 1] (machine-state-sequence [0 1] test-data)))

(deftest part-1-test
  (testing "The sum of the signal strengths at specific points is calculated correctly"
    (is (= 15220
           (reduce + (signal-strengths-at-times [20 60 100 140 180 220] test-state-seq))))))

(deftest part-2-test
  (is (= '("###..####.####.####.#..#.###..####..##.."
           "#..#.#.......#.#....#.#..#..#.#....#..#."
           "#..#.###....#..###..##...###..###..#..#."
           "###..#.....#...#....#.#..#..#.#....####."
           "#.#..#....#....#....#.#..#..#.#....#..#."
           "#..#.#....####.####.#..#.###..#....#..#.")
         
       (map (partial apply str) (partition 40 (map #(pixel-state-at-time % test-state-seq) (range 1 241)))))))
