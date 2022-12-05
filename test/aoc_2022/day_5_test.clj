(ns aoc-2022.day-5-test
  (:require [clojure.test :refer :all]
            [aoc-2022.core :refer :all]
            [aoc-2022.day-5 :refer :all]))


(deftest moving-between-stacks-test
  (testing "items can be moved one by one between stacks"
    (is (= (move-1  '(\D \C \M) '(\N \Z))
           ['(\C \M) '(\D \N \Z)]))))

(deftest moving-between-numbered-stacks-test
  (testing "items can be moved one by one between numbered stacks"
    (let [start-state ['(\N \Z)
                       '(\D \C \M)
                       '(\P)]]

      (is (= ['(\D \N \Z)
              '(\C \M)
              '(\P)]
             (move-1-in start-state 2 1))))))

(deftest moving-more-than-one-between-numbered-stacks-test
  (testing "items can be moved between numbered stacks in batches"
    (let [start-state ['(\D \N \Z)
                       '(\C \M)
                       '(\P)]]

      (is (= ['()
              '(\C \M)
              '(\Z \N \D \P)]
             (move-in start-state 3 1 3))))))

(deftest multiple-move-commands-test
  (testing "multiple multiple-move commands can be executed correctly"
    (let [start-state ['(\N \Z)
                       '(\D \C \M)
                       '(\P)]]

      (is (= ['(\C)
              '(\M)
              '(\Z \N \D \P)]
             
             (-> start-state
                 (move-in 1 2 1)
                 (move-in 3 1 3)
                 (move-in 2 2 1)
                 (move-in 1 1 2)))))))

(deftest multiple-move-command-sequences-test
  (testing "sequences of multiple-move commands can be executed correctly"
    (let [start-state ['(\N \Z)
                       '(\D \C \M)
                       '(\P)]]

      (is (= ['(\C)
              '(\M)
              '(\Z \N \D \P)]
             
             (apply-moves start-state
                          ['(1 2 1)
                           '(3 1 3)
                           '(2 2 1)
                           '(1 1 2)]))))))


;; problems

(deftest command-parsing-test
  (testing "command lines are parsed into arguments for move-in"
    (is (= [3 4 3] (parse-move "move 3 from 4 to 3")))))

(def test-data
  (map parse-move (lines-as-vector "input-5-moves.txt")))

(defn make-stack [stack-string]
  (reverse (into '() (seq stack-string))))


(deftest moving-substacks-between-numbered-stacks-test
  (testing "items can be moved between numbered stacks in substacks"
    (let [start-state ['(\D \N \Z)
                       '(\C \M)
                       '(\P)]]

      (is (= ['()
              '(\C \M)
              '(\D \N \Z \P)]
             (move-in-chunk start-state 3 1 3))))))


(def test-initial-state
  ;; this will teach me to hand-enter my starting state :(
  ;; turns out string-seqs aren't stacks
  (into [] (map make-stack [(seq "WRTG")
                            (seq "WVSMPHCG")
                            (seq "MGSTLC")
                            (seq "FRWMDHJ")
                            (seq "JFWSHLQP")
                            (seq "SMFNDJP")
                            (seq "JSCGFDBZ")
                            (seq "BTR")
                            (seq "CLWNH")])))


(deftest part-1-test
  (is (= "JCMHLVGMG"
         (apply str
                (map first (apply-moves test-initial-state test-data))))))

(deftest part-2-test
  (is (= "LVMRWSSPZ"
         (apply str
                (map first (apply-chunked-moves test-initial-state test-data))))))
