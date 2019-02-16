(ns advent-clojure.day14
  (:require [clojure.string :as str]
            [advent-clojure.day10 :as day10]))

(defn to-binary
  [number]
  (let [str-val (Integer/toString number 2)]
    (day10/pad-0s str-val 4)))

(defn knot-hash-bits
  [input]
  (let [knot-hash (day10/get-knot-hash input)]
    (->> (day10/pad-0s knot-hash 32)
         (map #(Integer/parseInt (str %) 16))
         (map to-binary)
         (reduce str))))

(defn create-map
  [binary-str y]
  (map (fn [str-val x]
         {[x y] {:pos [x y] :val str-val}})
       binary-str
       (range (count binary-str))))
  
(defn count-1s
  [input-str]
  (->> input-str
       (filter #(= % \1))
       (count)))

(def PUZZLE_HEIGHT 128)

(defn get-knot-hash-bit-strings
  [input]
  (let [inputs (map #(str input "-" %) (range PUZZLE_HEIGHT))]
    (map knot-hash-bits inputs)))

(defn puzzle-14-2
  [input]
  (let [knot-bits (get-knot-hash-bit-strings input)]
    (map create-map knot-bits (range PUZZLE_HEIGHT))))

(defn puzzle-14-1
  [input]
  (->> (get-knot-hash-bit-strings input)
       (map count-1s)
       (reduce +)))
    
