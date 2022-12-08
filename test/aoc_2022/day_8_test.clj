(ns aoc-2022.day-8-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [aoc-2022.core :refer :all]
            [aoc-2022.day-8 :refer :all]))


(def toy-data
  "30373
25512
65332
33549
35390")

(def toy-forest 
  (into [] (map #(into [] (map ctoi %)) (str/split toy-data #"\n"))))

(deftest integer-test
  (testing "Characters are translated to integers correctly (clojure does not allow you to compare chars"
    (is (= 5 (ctoi \5)))
    (is (= 8 (ctoi \8)))
    (is (= 0 (ctoi \0)))))


(deftest tree-height-test
  (testing "Tree height are retrieved from the forest"
    (is (= 3 (tree-height toy-forest 0 0)))
    (is (= 5 (tree-height toy-forest 1 1)))
    (is (= 6 (tree-height toy-forest 0 2)))))

(deftest toy-visibility-test
  (testing "The top-left 5 is visible from the top and the left"
    (is (visible-from-top? toy-forest 1 1))
    (is (visible-from-left? toy-forest 1 1))
    (is (not (visible-from-bottom? toy-forest 1 1)))
    (is (not (visible-from-right? toy-forest 1 1))))
  (testing "The top-middle 5 is visible from the top and the left"
    (is (visible-from-top? toy-forest 2 1))
    (is (not (visible-from-left? toy-forest 2 1)))
    (is (not (visible-from-bottom? toy-forest 2 1)))
    (is (visible-from-right? toy-forest 2 1)))
  (testing "The left-middle 5 is visible only from the right"
    (is (not (visible-from-top? toy-forest 1 2)))
    (is (not (visible-from-left? toy-forest 1 2)))
    (is (not (visible-from-bottom? toy-forest 1 2)))
    (is (visible-from-right? toy-forest 1 2)))
  (testing "The bottom middle 5 is visible"
    (is (not (visible-from-top? toy-forest 2 4)))
    (is (not (visible-from-left? toy-forest 2 4)))
    (is (visible-from-bottom? toy-forest 2 4))
    (is (not (visible-from-right? toy-forest 2 4)))))

;; partial linear solution, even more code, boring but effective
;; still too boring to do the whole thing though

(deftest toy-visibility-test-with-cache
  (let [left-map (visibility-from-left-forest-map toy-forest)]
    (testing "everything on the left is visible"
      (is (visible-from-left-mapped? left-map 0 4)))
    (testing "The top-left 5 is visible from the left"
      (is (visible-from-left-mapped? left-map 1 1)))
    (testing "The top-middle 5 is not visible from the left"
      (is (not (visible-from-left-mapped? left-map 2 1))))
    (testing "The left-middle 5 is not visible from the left"
      (is (not (visible-from-left-mapped? left-map 1 2))))
    (testing "The bottom middle 5 is visible"
      (is (not (visible-from-left-mapped? left-map 2 4))))))

(deftest visible-count-test
  (testing "a forest can have its visible trees counted"
    (is (= 21 (count-visible-trees toy-forest)))))

(deftest viewing-distance-test
  (testing "the middle 5 at the top has correct viewing distances"
    (is (= 1 (viewing-distance-up toy-forest 2 1)))
    (is (= 2 (viewing-distance-down toy-forest 2 1)))
    (is (= 1 (viewing-distance-left toy-forest 2 1)))
    (is (= 2 (viewing-distance-right toy-forest 2 1))))
  (testing "the middle 5 at the bottom has correct viewing distances"
    (is (= 2 (viewing-distance-up toy-forest 2 3)))
    (is (= 1 (viewing-distance-down toy-forest 2 3)))
    (is (= 2 (viewing-distance-left toy-forest 2 3)))
    (is (= 2 (viewing-distance-right toy-forest 2 3)))))

(deftest scenic-score-test
  (testing "The highest scenic score is found"
    (is (= 8 (highest-scenic-score toy-forest)))))

(deftest best-scenic-score-test
  (testing "T"))
;; Problems

(def test-forest 
  (into [] (map #(into [] (map ctoi %))
                (lines-as-vector "input-8.txt"))))

(deftest part-1-test
  (is (= 1818
         (count-visible-trees test-forest))))
