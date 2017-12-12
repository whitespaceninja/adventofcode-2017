(ns advent-clojure.day11
  (:require [clojure.string :as str]))

(def move-amt
  {"ne" [1 -0.5]
   "se" [1 0.5]
   "s" [0 1]
   "sw" [-1 0.5]
   "n" [0 -1]
   "nw" [-1 -0.5] })

(defn walk
  [[x y] directions max-dist]
  (let [dir (first directions)
        directions (rest directions)
        move-mod (get move-amt dir)
        new-x (+ x (first move-mod))
        new-y (+ y (second move-mod))
        cur-dist (get-steps-away [new-x new-y])
        max-dist (max max-dist cur-dist)]
    (if (= (count directions) 0)
      {:max max-dist :pos [new-x new-y]}
      (recur [new-x new-y] directions max-dist))))

(defn get-to-center-line
  [a [x y]]
  (if (> x 0)
    (let [move-amt (get move-amt "nw")]
      (recur (inc a) [(+ x (first move-amt))
                      (+ y (second move-amt))]))
    [a [x y]]))

(defn get-steps-away
  [[x y]]
  (let [abs-x (Math/abs x)
        abs-y (Math/abs y)
        steps-to-center (get-to-center-line 0 [abs-x abs-y])
        moved-already (first steps-to-center)
        new-pos (second steps-to-center)]
    (+ (second new-pos) moved-already)))

(defn puzzle-10-1
  [input]
  (let [directions (str/split input #",")
        walked (walk [0 0] directions 0)
        final-pos (:pos walked)
        max-steps (:max walked)
        steps-away (get-steps-away final-pos)]
    (println (str "final-pos: " final-pos " is " steps-away " steps-away and max-dist: " max-steps))))

