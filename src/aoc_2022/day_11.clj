(ns aoc-2022.day-11
  (:require [clojure.string :as str]
            [aoc-2022.core :refer :all]))

(defn square [n]
  (* n n))

;; I promised myself I was going to do the timple thing for day 11. I lied.
;; eg ((apply partial (:operation m)) 2)
(defn make-op [op-string arg-string]
  (if (= ["*" "old"] [op-string arg-string])
    (list (resolve (symbol "square")))
    (list (resolve (symbol op-string)) (Integer/parseInt arg-string))))

;; I hate parsing text so much
(defn parse-monkey [monkey-string]
  {:number (Integer/parseInt (second (re-find #"Monkey (\d)" monkey-string)))
   :items (apply vector (intify-seq (str/split (second (re-find #"Starting items: (.*)\n" monkey-string)) #", ")))
   :operation (apply make-op (rest (re-find #"Operation: new = old ([\+\*]) (\S+)" monkey-string)))
   :test-divisible (Integer/parseInt (second (re-find #"divisible by (\d+)" monkey-string)))
   :true-monkey (Integer/parseInt (second (re-find #"If true: throw to monkey (\d+)" monkey-string)))
   :false-monkey (Integer/parseInt (second (re-find #"If false: throw to monkey (\d+)" monkey-string)))
   :inspected-items 0})


(defn process-monkey-items [monkey monkeys]
  
  (let [items (:items monkey)]

    (if (empty? items)
      (assoc monkeys (:number monkey) monkey)

      (let [item (first items)
            new-item-value ((apply partial (:operation monkey)) item)
            adjusted-item-value (quot new-item-value 3)
            inspected-items (:inspected-items monkey)
            monkey' (-> monkey
                        (assoc :items (apply vector (rest items)))
                        (update-in [:inspected-items] inc))]
        
        (if (= 0 (mod adjusted-item-value (:test-divisible monkey)))
          (recur monkey' (update-in monkeys [(:true-monkey monkey) :items] conj adjusted-item-value))
          (recur monkey' (update-in monkeys [(:false-monkey monkey) :items] conj adjusted-item-value)))))))

(defn process-all-monkeys
  ([monkeys]
   (process-all-monkeys 0 monkeys))
  ([index monkeys]
   (if (= index (count monkeys))
     monkeys
     (recur (inc index) (process-monkey-items (get monkeys index) monkeys)))))

(defn after-rounds [monkeys rounds]
  (nth (iterate process-all-monkeys monkeys) rounds))

