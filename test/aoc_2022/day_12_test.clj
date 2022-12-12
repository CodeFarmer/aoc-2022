(ns aoc-2022.day-12-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [aoc-2022.core :refer :all]
            [aoc-2022.day-12 :refer :all]))

(def toy-map
  (apply vector
         (str/split
          "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"
          #"\n")))

(deftest height-difference-test
  (testing "elevations are the correct numbers of steps apart"
    (is (= 25 (- (elevation [4 2] toy-map)
                 (elevation [0 0] toy-map))))))

(deftest adjacent-points-test
  (testing "adjacent points to a space in the middle are identified"
    (is (= '([2 3] [3 2] [3 4] [4 3])
           (adjacent-points [3 3] toy-map))))
  (testing "only adjacent points that exist are identified when we are near the edge"
    (is (= '([0 1] [1 0])
           (adjacent-points [0 0] toy-map)))
    (is (= '([6 4] [7 3])
           (adjacent-points [7 4] toy-map)))))

(deftest possible-paths-from-test
  (testing "adjacent points on the same level are possible"
    (is (= [[0 1] [1 0]]
           (possible-points [0 0] toy-map))))
  (testing "adjacent points that are more than two steps higher are excluded"
    (is (= [[1 0]
            ;; not [3 0]
            [2 1]]
           (possible-points [2 0] toy-map)))))

(def test-map
  (lines-as-vector "input-12.txt"))

(deftest find-start-test
  (testing "The start of the toy data is located correctly"
    (is (= [0 0] (find-start toy-map))))
  (testing "The start of the test data is located correctly"
    (is (= [0 20] (find-start test-map)))))

;; problems

(deftest breadth-first-search-test
  (testing "the shortest legal path in the toy data is found"
    ;; my path inludes the start, so 31 steps
    (is (= 32 (count (shortest-legal-path toy-map))))))

