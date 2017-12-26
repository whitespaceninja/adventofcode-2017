(ns advent-clojure.day25
  (:require [clojure.string :as str]))

(def INPUT {"A" {0 {:val 1
                    :move 1
                    :next-state "B"}
                 1 {:val 0
                    :move -1
                    :next-state "B"}}
            "B" {0 {:val 0
                    :move 1
                    :next-state "C"}
                 1 {:val 1
                    :move -1
                    :next-state "B"}}
            "C" {0 {:val 1
                    :move 1
                    :next-state "D"}
                 1 {:val 0
                    :move -1
                    :next-state "A"}}
            "D" {0 {:val 1
                    :move -1
                    :next-state "E"}
                 1 {:val 1
                    :move -1
                    :next-state "F"}}
            "E" {0 {:val 1
                    :move -1
                    :next-state "A"}
                 1 {:val 0
                    :move -1
                    :next-state "D"}}
            "F" {0 {:val 1
                    :move 1
                    :next-state "A"}
                 1 {:val 1
                    :move -1
                    :next-state "E"}}})

(defn get-node
  [pos nodes]
  (if (contains? nodes pos)
    (get nodes pos)
    0))
    

(defn walk
  [cursor tape n]
  (let [cur-pos (:pos cursor)
        cur-state (get INPUT (:state cursor))
        cur-node (get-node cur-pos tape)
        instruct (get cur-state cur-node)
        next-cursor (-> cursor
                        (assoc :pos (+ cur-pos (:move instruct)))
                        (assoc :state (:next-state instruct)))
        tape (assoc tape cur-pos (:val instruct))
        n (- n 1)]
    (if (= n 0)
      tape
      (recur next-cursor tape n))))

(defn puzzle-25-1
  [num-steps]
  (let [tape (walk {:pos 0 :state "A"} {} num-steps)]
    (->> tape
         vals
         (reduce +))))
       
