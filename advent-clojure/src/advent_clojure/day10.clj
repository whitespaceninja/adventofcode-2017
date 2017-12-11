(ns advent-clojure.day10
  (:require [clojure.string :as str]))

(def real-rope (vec (range 0 256)))
(def test-rope (vec (range 0 5)))

(defn add-within
  [rope & nums-to-add]
  (mod (apply + nums-to-add) (count rope)))

(defn cut-and-tie
  [rope at-indx]
  (vec (concat (subvec rope at-indx)
               (take at-indx rope))))
  

(defn twist
  [skip-size rope lengths offset]
  (let [length (first lengths)
        lengths (rest lengths)
        reversed (reverse (take length rope))
        reversed-rope (vec (concat reversed (subvec rope length)))
        new-start (add-within rope skip-size length)
        new-rope (cut-and-tie reversed-rope new-start)
        new-offset (add-within rope
                               offset
                               (- (count rope)
                                  (+ skip-size length)))
        new-skip-size (+ 1 skip-size)]
    (if (<= (count lengths) 0)
      {:rope new-rope :offset new-offset :skip-size new-skip-size}
      (recur new-skip-size new-rope lengths new-offset))))

(defn parse-input
  [input]
  (->> (str/split input #",")
       (map read-string)
       vec))

(defn twist-n-times
  [rope skip-size offset lengths n]
  (let [answer (twist skip-size rope lengths offset)
        _ (println (str "n " n " answer: " answer))]
    (if (= n 1)
      answer
      (recur (:rope answer)
             (:skip-size answer)
             (:offset answer)
             lengths
             (- n 1)))))


(def HARD_END [17 31 73 47 23])

(defn puzzle-10-2
  [rope input]
  (let [lengths (concat (map int (char-array input)) HARD_END)
        after-twists (twist-n-times rope 0 0 lengths 64)
        rope-reset (cut-and-tie (:rope after-twists) (:offset after-twists))
        partitions (partition 16 rope-reset)
        partitions-xor (map #(apply bit-xor %) partitions)
        hexed (map #(format "%x" %) partitions-xor)]
    (reduce str hexed)))

(defn puzzle-10-1
  [rope input]
  (let [lengths (parse-input input)
        answer (twist 0 rope lengths 0)
        final-rope (:rope answer)
        offset (:offset answer)
        first-char (nth final-rope offset)
        second-char (nth final-rope (add-within final-rope 1 offset))
        mult (* first-char second-char)]
    [mult answer]))
    
     
