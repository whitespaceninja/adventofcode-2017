(ns advent-clojure.core
  (:gen-class))

(defn get-single-char
  [input-string start-index]
  (subs input-string start-index (+ start-index 1)))

(defn get-string-pair
  [input-string start-index num-to-add]
  (let [first-char (get-single-char input-string start-index)
        next-char-index-raw (+ start-index num-to-add)
        next-char-index (mod next-char-index-raw (count input-string))
        second-char (get-single-char input-string next-char-index)]
     [first-char second-char]))

(defn convert-to-int
  ;; takes a pair of characters and converts them to integers
  [pair]
  (map #(read-string %) pair))

(defn count-pairs
  [input-string look-ahead]
  (let [index-range (range (count input-string))]
    (->> (map #(get-string-pair input-string % look-ahead) index-range)
         (map #(convert-to-int %))
         (filter #(= (first %) (second %)))
         (map first)
         (reduce +))))

(defn puzzle-1-1
  [input-string]
  (count-pairs input-string 1))

(defn puzzle-1-2
  [input-string]
  (count-pairs input-string (/ (count input-string) 2)))
  
(defn -main
  [input]
  (println (str "Puzzle 1-1: " (puzzle-1-1 input)))
  (println (str "Puzzle 1-2: " (puzzle-1-2 input))))
