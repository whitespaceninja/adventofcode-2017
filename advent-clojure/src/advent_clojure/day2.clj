(ns advent-clojure.day2
  (:require [clojure.string :as str]))

(defn convert-to-ints
  [list-of-str]
  (map #(read-string %) list-of-str))

(defn filter-to-sm-lg
  [item-list]
  [(apply min item-list)
   (apply max item-list)])

(defn convert-input-to-ints
  [input]
  (->> (str/split input #"\n")
       (map #(str/split % #"\t"))
       (map convert-to-ints)))

(defn check-divisible-against-others
  [int-list index]
  (let [num-count (count int-list)
        this-num (nth int-list index)
        other-nums (take-last (- num-count index 1) int-list)
        divisible (filter #(= 0 (mod (/ this-num %) 1)) other-nums)]
    (if (empty? divisible)
      nil
      [this-num (first divisible)])))

(defn find-divisible-pair
  [int-list]
  (let [num-count (count int-list)]
    (->> (map #(check-divisible-against-others int-list %)
              (range num-count))
         (filter #(not (empty? %)))
         first)))

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
