(ns aoc-2022.day-11-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [aoc-2022.core :refer :all]
            [aoc-2022.day-11 :refer :all]))

(def toy-monkey-strings
  (str/split
   "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1" #"\n\n"))

(deftest parse-monkey-test
  (testing "a monkey is parsed into an object correctly"
    (is (= {:number 0
            :items [79 98]
            :operation (list #'clojure.core/* 19)
            :test-divisible 23
            :true-monkey 2
            :false-monkey 3
            :inspected-items 0}
           (parse-monkey (first toy-monkey-strings))))))

(def toy-monkeys (apply vector (map parse-monkey toy-monkey-strings)))


(def test-monkeys (apply vector (map parse-monkey
                                     (str/split (slurp "input-11.txt") #"\n\n"))))

;; problems

(deftest part-1-test
  (testing "the level of monkey business is crrectly calculated as the product of the numbers of inspected items of the two most active monkeys after 20 rounds"
    (is (= 118674
           (apply * (map :inspected-items (take 2 (sort-by :inspected-items > (after-rounds test-monkeys 20)))))))))
