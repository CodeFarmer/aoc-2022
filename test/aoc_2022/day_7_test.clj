(ns aoc-2022.day-7-test
  (:require [clojure.test :refer :all]
            [aoc-2022.core :refer :all]
            [aoc-2022.day-7 :refer :all]))


(def toy-data
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

;; each element is now a vector whose first element is a command string, and whose following elements are the output lines
(def toy-commands
  (map #(str/split % #"\n")
       (map str/trim
            (rest (str/split toy-data #"\$")))))

(deftest leaf-dir-test
  (testing "From command output, a directory without subdirectories gets puplated correctly"
    (is (= 0 1))))

