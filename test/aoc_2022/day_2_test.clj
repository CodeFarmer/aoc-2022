(ns aoc-2022.day-2-test
  (:require [clojure.test :refer :all]
            [aoc-2022.day-2 :refer :all]
            [aoc-2022.core :refer :all]
            [clojure.string :as str]))

(deftest shape-scoring-test
  (testing "Shapes should be scored correctly"
    (is (= (shape-score :rock) 1))
    (is (= (shape-score :paper) 2))
    (is (= (shape-score :scissors) 3))
    (is (= (shape-score :nothing) 0))))

(deftest result-scoring-test
  (testing "Appropriate scores for win, lose or draw"
    (is (= (result-score :rock :paper) 6))
    (is (= (result-score :paper :rock) 0)
        (= (result-score :scissors :scissors) 3))))

(deftest round-scoring-with-encoded-choice-test
  (testing "Rounds should be scored correctly when the encoding represents a specific move"
    (is (= (round-score-predetermined "A" "Y") 8))
    (is (= (round-score-predetermined "B" "X") 1))
    (is (= (round-score-predetermined "C" "Z") 6))))


(deftest round-scoring-with-deliberate-outcome-test
  (testing "Rounds should be scored correctly when the outcome is predetermined"
    (is (= (round-score-deliberate "A" "Y") 4))
    (is (= (round-score-deliberate "B" "X") 1)
        (= (round-score-deliberate "C" "Z") 7))))

;; problems

(def test-data
  (map #(str/split % #" ") (lines-as-vector "input-2.txt")))

(deftest part-1-test
  (testing "The total should be correct when using the predetermined version of the choice encoding"
    (is (= (reduce + (map (partial apply round-score-predetermined)
                          test-data))
           11150))))

(deftest part-2-test
  (testing "The total should be correct when using the predetermined version of the choice encoding"
    (is (= (reduce + (map (partial apply round-score-deliberate)
                          test-data))
           8295))))

