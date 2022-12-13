(ns aoc-2022.day-13-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [aoc-2022.core :refer :all]
            [aoc-2022.day-13 :refer :all]))

(def toy-data
  "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

(def toy-pairs
  (map #(map edn/read-string (str/split % #"\n")) (str/split toy-data #"\n\n")))

(deftest lless-than-test
  (testing "Integers are tested correctly"
    (is (lless-than 0 1))
    (is (not (lless-than 1 0))))
  (testing "Lists of integers are tested correctly"
    (is (lless-than [1 2 3 4] [2 3 4 5]))
    (is (not (lless-than [2 3 4 5] [1 2 3 4]))))
  (testing "Demo lists are compared correctly"
    (is (lless-than [[1],[2,3,4]] [[1],4]))
    (is (not (lless-than [9] [[8,7,6]])))
    (is (lless-than [[4,4],4,4] [[4,4],4,4,4]))
    (is (not (lless-than [7,7,7,7] [7,7,7])))
    (is (not (lless-than [[[]]] [[]]))))
  (testing "the problematic input pair is orderd correctly"

    (is (not (lless-than [[] [[[3] 10 3 7]] []]
                         [[] [[] 4 [2 [0] 0 []] 4] [] [[5 0 10 6 9] 2]])))))


(deftest ordered-indices-test
  (testing "the indices are returned correctly for the correctly-ordered pairs in the toy data"
    (is (= [1 2 4 6] (ordered-indices toy-pairs))))
  (testing "the sum of the ordered indices in the toy data is calculated correctly"
    (is (= 13 (apply + (ordered-indices toy-pairs))))))

;; problems

(def test-pairs (map #(map edn/read-string (str/split % #"\n")) (str/split (slurp "input-13.txt") #"\n\n")))

(deftest part-1-test
  (testing "The correct indices that are already ordered are identified in the test data"
    (is (= 5843 (reduce + (ordered-indices test-pairs))))))

(def test-packets (into '([[2]] [[6]])
                        (map edn/read-string (filter (complement empty?) (str/split (slurp "input-13.txt") #"\n")))))

(deftest part-2-test
  (testing "The decoder key for the test data is correctly found"
    (is (= 26289
           (apply * (indices-matching #{[[2]] [[6]]} (sort lless-than test-packets)))))))
