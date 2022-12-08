(ns aoc-2022.day-7
  (:require [aoc-2022.core :refer :all]
            [clojure.string :as str]))

;; FIXME not only was this not done on the day, it's totally clunky
;; and depressing. All the stuff with parsing into commands and chunks
;; and so on is terrible.

(defn split-on-spaces [aseq]
  (map #(str/split % #" ") aseq))

(defn ls [output-lines]
  (into {}
        (map (fn [[info filename]]
               (if (= "dir" info)
                 [filename {}]
                 [filename (Integer/parseInt info)])) output-lines)))


(defn cd [path-stack arg]
  (case arg
    ".." (pop path-stack)
    "/" '()
    (conj path-stack arg)))

(defn process-command [path-stack dir-tree command-seq]
  (let [[[command & args] & output] command-seq]
    (case command
      "cd"
      [(cd path-stack (first args)) dir-tree]
      "ls"
      [path-stack
       (if (empty? path-stack) ;; this is ugly but assoc-in is weird
         (ls output)
         (assoc-in dir-tree (reverse path-stack) (ls output)))])))


(defn process-commands
  ([command-seq]
   (process-commands '() {} command-seq))
  ([path-stack dir-tree command-seq]
   (if (empty? command-seq)
     [path-stack dir-tree]
     (let [[path' dir'] (process-command path-stack dir-tree (first command-seq))]
       (recur path' dir' (rest command-seq))))))

(defn directory-size [directory-map]
  (reduce + (map (fn [[name info]]
                   (if (map? info)
                     (directory-size info)
                     info))
                 (seq directory-map))))

(defn find-all-dirs
  ([dir-tree]
   (find-all-dirs [] dir-tree))
  ([found-dirs dir-tree]
   (let [subdirs (map second (filter (fn [[name thing]] (map? thing)) (seq dir-tree)))]
     
     (conj 
      (reduce concat found-dirs (map find-all-dirs subdirs))
      dir-tree))))
