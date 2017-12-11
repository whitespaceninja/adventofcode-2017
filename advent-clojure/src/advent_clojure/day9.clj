(ns advent-clojure.day9
  (:require [clojure.string :as str]))

(defn remove-escaped
  [input]
  (let [next-start (or (str/index-of input "!") -1)
        next-end (+ 2 next-start)]
    (if (>= next-start 0)
      (str (subs input 0 next-start)
           (remove-escaped (subs input next-end)))
      input)))

(defn remove-garbage
  [input]
  (let [next-start (or (str/index-of input "<") -1)]
    (if (>= next-start 0)
      (let [next-end (+ 1 (str/index-of input ">" (+ 1 next-start)))]
        (str (subs input 0 next-start)
             (remove-garbage (subs input next-end))))
      input)))

 (defn count-garbage
  [acc input]
  (let [next-start (or (str/index-of input "<") -1)]
    (if (>= next-start 0)
      (let [next-end (+ 1 (str/index-of input ">" (+ 1 next-start)))
            num-garbage (- next-end next-start 2)]
        (count-garbage (+ acc num-garbage)
                       (str (subs input 0 next-start)
                            (subs input next-end))))
      acc)))

(defn walk-c
  [acc level input]
  (let [ch (first input)
        input (rest input)]
    (case ch
      \{ (+ acc level (walk-c acc (+ level 1) input))
      \} (+ acc (walk-c acc (- level 1) input))
      \, (+ acc (walk-c acc level input))
      acc)))

(defn puzzle-9-2
  [input]
  (->> input
       remove-escaped
       (count-garbage 0)))

(defn puzzle-9-1
  [input]
  (->> input
       remove-escaped
       remove-garbage
       (walk-c 0 1)))
               
