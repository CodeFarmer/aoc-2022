(ns aoc-2022.day-4-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [aoc-2022.core :refer :all]
            [aoc-2022.day-4 :refer :all]))


(deftest range-containment-test
  (testing "a range is wholly contained in another range"
    (is (not (wholly-contains? [2 4] [6 8])))
    (is (not (wholly-contains? [5 7] [7 9])))
    (is (wholly-contains? [2 8] [3 7]))
    (is (wholly-contains? [4 6] [6 6]))
    (is (not (wholly-contains? [6 6] [4 6])))))

(deftest either-range-containment-test
  (testing "either of two ranges wholly contains the other"
    (is (not (either-contains? [2 4] [6 8])))
    (is (either-contains? [4 6] [6 6]))
    (is (either-contains? [6 6] [4 6]))))

(def mini-test-data
  [[[2 4] [6 8]]
   [[2 3] [4 5]]
   [[5 7] [7 9]]
   [[2 8] [3 7]]
   [[6 6] [4 6]]
   [[2 6] [4 8]]])

(deftest overlapping-range-counting-test
  (testing "For the toy example, overlapping ranges are counted correctly"
    (is (= 2 (count-overlapping-ranges mini-test-data)))))


(def test-data
  (map #(partition 2 (intify-seq (str/split % #"[,-]")))
       (lines-as-vector "input-4.txt")))

(deftest part-1-test
  (is (= 528 (count-overlapping-ranges test-data))))

(deftest part-2-test)
