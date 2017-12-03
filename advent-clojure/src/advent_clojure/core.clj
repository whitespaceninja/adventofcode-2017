(ns advent-clojure.core
  (:require
   [advent-clojure.day1 :as day1])
  (:gen-class))
  
(defn -main
  [input]
  (time (println (str "Puzzle 1-1: " (day1/puzzle-1-1 input))))
  (time (println (str "Puzzle 1-2: " (day1/puzzle-1-2 input)))))
