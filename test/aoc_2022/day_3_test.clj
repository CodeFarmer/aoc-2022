(ns aoc-2022.day-3-test
  (:require [clojure.test :refer :all]
            [aoc-2022.core :refer :all]
            [aoc-2022.day-3 :refer :all]))


(deftest rucksack-splitting-test
  (testing "lines should be corrrectly divided into rucksack compartments"
    (is (= [(seq "vJrwpWtwJgWr") (seq "hcsFMMfFFhFp")]
           (split-rucksack "vJrwpWtwJgWrhcsFMMfFFhFp")))
    (is (= [(seq "jqHRNqRjqzjGDLGL") (seq "rsFMfFZSrLrFZsSL")]
           (split-rucksack "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")))
    (is (= [(seq "PmmdzqPrV") (seq "vPwwTWBwg")]
           (split-rucksack "PmmdzqPrVvPwwTWBwg")))))


(deftest common-item-test
  )


(deftest rucksack-common-item-test
  (testing "rucksack strings should have their compartments' common items identified"
    (is (= \v (rucksack-common-item "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn")))
    (is (= \t (rucksack-common-item "ttgJtRGJQctTZtZT")))
    (is (= \s (rucksack-common-item "CrZsJsPPZsGzwwsLwLmpwMDw")))))


(deftest item-priority-test
  (testing "items should have their priority correctly assigned"
    (is (= 1 (item-priority \a)))
    (is (= 26 (item-priority \z)))
    (is (= 27 (item-priority \A)))
    (is (= 52 (item-priority \Z)))))

(deftest rucksack-item-priority-test
  (testing "rucksack strings should have their common item priorities correctly scored"
    (is (= 16 (rucksack-common-item-priority "vJrwpWtwJgWrhcsFMMfFFhFp")))
    (is (= 38 (rucksack-common-item-priority "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")))
    (is (= 42 (rucksack-common-item-priority "PmmdzqPrVvPwwTWBwg")))))

(deftest common-item-seq-test

  (testing "the common item between two rucksack compartments should be identified"
    (is (= \p (common-item [(seq "vJrwpWtwJgWr") (seq "hcsFMMfFFhFp")])))
    (is (= \L (common-item [(seq "jqHRNqRjqzjGDLGL") (seq "rsFMfFZSrLrFZsSL")]))))
  
  (testing "The common item should be correctly identified for sequences of strings"
    (is (= \r (common-item ["vJrwpWtwJgWrhcsFMMfFFhFp"
                            "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                            "PmmdzqPrVvPwwTWBwg"])))
    (is (= \Z (common-item ["wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                            "ttgJtRGJQctTZtZT"
                            "CrZsJsPPZsGzwwsLwLmpwMDw"])))))

;; problems


(def test-data
  (lines-as-vector "input-3.txt"))


(deftest part-1-test
  (testing "the right answer for question 1 is calculated"
    (is (= 8018
           (reduce + (map rucksack-common-item-priority test-data))))))

(deftest part-2-test
  (testing "the right answer for question 2 is calculated"
    (is (= 2518
           (reduce + (map item-priority (map common-item (partition 3 test-data))))))))
