(ns aoc-2022.day-13
  (:require [clojure.string :as str]
            [aoc-2022.core :refer :all]))


(defn definitely-a-seq [a]
  (if (int? a)
    (list a)
    a))


(defn lless-than [a b]
  (if (and (int? a) (int? b))
    (if (= a b)
      0
      (< a b))
    (let [a' (definitely-a-seq a)
          b' (definitely-a-seq b)]
      (cond
        (empty? a') (if (empty? b') 0 ;; there is one of the 150 test pairs where this matters
                        true)
        (empty? b') false

        :default (let [ltf (lless-than (first a') (first b'))]
                   (if (not (= 0 ltf))
                     ltf
                     (lless-than (rest a') (rest b'))))))))


(defn ordered-indices
  ([pair-list]
   (ordered-indices [] 1 pair-list))
  ([acc index pair-list]
   (if (empty? pair-list)
     acc
     (if (apply lless-than (first pair-list))
       (recur (conj acc index) (inc index) (rest pair-list))
       (recur acc (inc index) (rest pair-list))))))

(defn indices-matching
  "Return 1-based indices for all the items in aseq where fn returns truthy"
  ([fn aseq]
   (indices-matching [] 1 fn aseq))
  ([acc index fn aseq]
   (if (empty? aseq)
     acc
     (if (fn (first aseq))
       (recur (conj acc index) (inc index) fn (rest aseq))
       (recur acc (inc index) fn (rest aseq))))))
