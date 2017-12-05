(ns advent-clojure.day5
  (:require [clojure.string :as str]))

(defn convert-input-to-ints
  [input]
  (->> (str/split input #"\n")
       (map read-string)))

(defn out-of-index
  [index step-list]
  (or (< index 0)
      (>= index (count step-list))))

(defn normal-jump
  [x]
  (+ 1 x))

(defn stranger-jump
  [x]
  (if (>= x 3)
    (- x 1)
    (+ x 1)))

(defn walk
  [cur-index num-steps jump-fn step-list]
  (let [step (nth step-list cur-index)
        next-index (+ cur-index step)
        num-steps (+ 1 num-steps)]
    (if (out-of-index next-index step-list)
      num-steps
      (let [updated-list (assoc step-list cur-index (jump-fn step))]
        (recur next-index num-steps jump-fn updated-list)))))

(defn puzzle-5-2
  [input]
  (->> (convert-input-to-ints input)
       (vec)
       (walk 0 0 stranger-jump)))

(defn puzzle-5-1
  [input]
  (->> (convert-input-to-ints input)
       (vec)
       (walk 0 0 normal-jump)))
