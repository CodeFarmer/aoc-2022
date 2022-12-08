(ns aoc-2022.day-7-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
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
  (map split-on-spaces
       (map #(str/split % #"\n")
            (map str/trim
                 (rest (str/split toy-data #"\$"))))))

(deftest command-parsing-test
  (testing "From command output, a directory without subdirectories gets puplated correctly"
    (is (= '(["ls"] ["dir" "e"] ["29116" "f"] ["2557" "g"] ["62596" "h.lst"])
           (nth toy-commands 3)))))

(deftest directory-node-building-test
  (testing "an ls command in a directory with no subdirectories builds the right map"
    (is (= { "j"    4060174
            "d.log" 8033020
            "d.ext" 5626152
            "k"     7214296}
           (ls '(["4060174" "j"]
                 ["8033020" "d.log"]
                 ["5626152" "d.ext"]
                 ["7214296" "k"])))))
  
  (testing "An ls command in a directory with subdirectories puts placeholder maps in for them"
    (is (= {"a"     {}
            "d"     {}
            "b.txt" 14848514
            "c.dat" 8504156}
           (ls '(["dir" "a"]
                 ["14848514" "b.txt"]
                 ["8504156" "c.dat"]
                 ["dir" "d"]))))))

(deftest cd-test
  (testing "Given a directory tree and a current working directory stack, cd into a subdirectory by adding a new entry to the stack"
    (is (= '("b" "a")
           (cd '("a") "b"))))
  (testing "Changing directory upwards works as expected"
    (is (= '("a")
           (cd '("b" "a") ".."))))
  (testing "Changing to the root directory works as expected"
    (is (= '()
           (cd '("b" "a") "/")))))


(deftest command-procesing-test

  (testing "Given a current directory and a path stack, a cd command correctly updates the path stack"
    (is (= ['("b")
            { "a" 10
             "b" {}}]))
    (process-command '() 
                     { "a" 10
                      "b" {}}
                     [["cd" "b"]]))

  (testing "Given a current directory and a path stack, an ls command updates the directory tree"
    (is (= ['("b") { "a" 10
                    "b" { "c" 20 }
                    }]
           (process-command '("b")
                            { "a" 10
                             "b" {}}
                            [["ls"] ["20" "c"]]))))
  
  (testing "Given a current directory and path stack, a cd into a leaf node followed by an ls updates the path stack and directory tree"
    (comment (is (= ['("b") { "a" 10
                           "b" { "c" 20 }
                           }]
                  (process-commands '()
                                    { "a" 10
                                     "b" { "c" 20 }
                                     }
                                    [["cd" "b"]
                                     ["ls" ["20" "c"]]]))))))

(deftest directory-size-test
  
  (testing "Given a leaf node, the total size of the directory's files is returned"
    (is (= 24933642 (directory-size {"j"    4060174
                                     "d.log" 8033020 
                                     "d.ext" 5626152 
                                     "k"     7214296}))))
  (testing "Given a nested directory, the total size of the directorys' files is returned"

    (is (= 24933642 (directory-size {"j"    4060174
                                     "d.log" 8033020 
                                     "subdir" {"d.ext" 5626152 
                                               "k"     7214296}}))))

  (testing "Given the toy test data, the size is as reported in the problem statement"
    (is (= 48381165 (directory-size (second (process-commands toy-commands)))))))

;; Problems

(def test-commands
  
  (map split-on-spaces
       (map #(str/split % #"\n")
            (map str/trim
                 (rest (str/split (slurp "input-7.txt") #"\$"))))))

(def test-tree (second (process-commands '() {} test-commands)))

(deftest part-1-test
  (is (= 1454188
         (reduce + (filter #(< % 100000) (map directory-size (find-all-dirs test-tree)))))))

(deftest part-2-test
  (is (= 4183246
         (first (sort (filter #(> % (- 30000000 (- 70000000 (directory-size test-tree)))) (map directory-size (find-all-dirs test-tree))))))))
