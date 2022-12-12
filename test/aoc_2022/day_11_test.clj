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

(deftest process-monkey-items-test
  (testing "A single monkey's items are processed correctly"
    (is (= [[]
            [54 65 75 74]
            [79 60 97]
            [74 500 620]]
           (map :items (process-monkey-items (first toy-monkeys) toy-monkeys))))))

(deftest whole-round-test
  (testing "After a single round, all monkeys have redistribted their items correctly"
    (is (= [[20 23 27 26]
            [2080 25 167 207 401 1046]
            []
            []]
           (map :items (process-all-monkeys toy-monkeys))))))

(deftest after-rounds-test
  
  (testing "Items are correctly assigned after arbitrary numbers of rounds"
    (is (= [[491 9 52 97 248 34]
            [39 45 43 258]
            []
            []]
           (map :items (after-rounds toy-monkeys 4))))
    (is (= [[10 12 14 26 34]
            [245 93 53 199 115]
            []
            []]
           (map :items (after-rounds toy-monkeys 20)))))
  
  (testing "The two monkeys with the highest activity are identified and monkey business is calculated"
    (is (= 10605
           (monkey-business (after-rounds toy-monkeys 20))))))

;; problems
(def test-monkeys (apply vector (map parse-monkey
                                     (str/split (slurp "input-11.txt") #"\n\n"))))

(deftest part-1-test
  (testing "the level of monkey business is correctly calculated as the product of the numbers of inspected items of the two most active monkeys after 20 rounds"
    (is (= 118674
           (monkey-business (after-rounds test-monkeys 20))))))

(deftest part-2-test
  (testing "the level of monkey business is crrectly calculated as the product of the numbers of inspected items of the two most active monkeys after 10000 rounds, given the new method of reducing anxiety"
    (let [floor (apply * (map :test-divisible test-monkeys))]
        (is (= 32333418600
               (monkey-business (after-rounds test-monkeys 10000 #(mod % floor))))))))
