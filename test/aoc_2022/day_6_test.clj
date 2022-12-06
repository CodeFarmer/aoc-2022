(ns aoc-2022.day-6-test
  (:require [clojure.test :refer :all]
            [aoc-2022.core :refer :all]
            [aoc-2022.day-6 :refer :all]))


(deftest has-repeats-test
  (testing "Strings with repeated characters are detected"
    (is (not (has-repeats? "mjq")))
    (is (has-repeats? "mjqj"))))

(deftest non-repeating-substring-length-4-detection-test
  (testing "The first non-repeating string of 4 characters is detected"
    (is (= 7  (non-repeating-string-index  4 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")))
    (is (= 5  (non-repeating-string-index  4 "bvwbjplbgvbhsrlpgdmjqwftvncz")))
    (is (= 6  (non-repeating-string-index  4 "nppdvjthqldpwncqszvftbrmjlhg")))
    (is (= 10 (non-repeating-string-index  4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")))
    (is (= 11 (non-repeating-string-index  4 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")))))

(deftest non-repeating-substring-length-14-detection-test
  (testing "The first non-repeating string of 14 characters is detected"
    (is (= 19 (non-repeating-string-index  14 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")))
    (is (= 23 (non-repeating-string-index  14 "bvwbjplbgvbhsrlpgdmjqwftvncz")))
    (is (= 23 (non-repeating-string-index  14 "nppdvjthqldpwncqszvftbrmjlhg")))
    (is (= 29 (non-repeating-string-index  14 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")))
    (is (= 26 (non-repeating-string-index  14 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")))))

;; puzzles

(def test-data (slurp "input-6.txt"))

(deftest part-1-test
  (testing "the first non-prepeating sequence of 4 in the test data is indexed correctly for Part 1"
    (is (= 1361 (non-repeating-string-index 4 test-data)))))

(deftest part-2-test
  (testing "the first non-prepeating sequence of 14 in the test data is indexed correctly for Part 1"
    (is (= 3263 (non-repeating-string-index 14 test-data)))))

