(ns advent-clojure.day2
  (:require [clojure.string :as str]))

(defn convert-to-ints
  [list-of-str]
  (map read-string list-of-str))

(defn filter-to-sm-lg
  [item-list]
  [(apply min item-list)
   (apply max item-list)])

(defn convert-input-to-ints
  [input]
  (->> (str/split-lines input)
       (map #(str/split % #"\t"))
       (map convert-to-ints)))

(defn is-divisible?
  [x y]
  (= 0 (mod (/ x y) 1)))

(defn find-divisible-pair
  ;; assumes int-list is a list of ints in reverse order
  [int-list]
  (let [this-num (first int-list)
        other-nums (rest int-list)
        divisible (filter #(is-divisible? this-num %) other-nums)]
    (if (empty? divisible)
      (recur other-nums)
      [this-num (first divisible)])))

(defn puzzle-2-2
  [input]
  (->> input
       (convert-input-to-ints)
       (map sort)
       (map reverse)
       (map find-divisible-pair)
       (map #(/ (first %) (second %)))
       (reduce +)))

(defn puzzle-2-1
  [input]
  (->> input
       (convert-input-to-ints)
       (map filter-to-sm-lg)
       (map #(- (second %) (first %)))
       (reduce +)))
