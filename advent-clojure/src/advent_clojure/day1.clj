(ns advent-clojure.day1
  (:gen-class))

(defn get-single-char
  [input-str start]
  (subs input-str start (+ start 1)))

(defn get-next-char
  [input-str start to-add]
  (let [next-index-raw (+ start to-add)
        next-index (mod next-index-raw (count input-str))]
    (get-single-char input-str next-index)))

(defn get-string-pair
  [input-str start to-add]
  (let [first-char (get-single-char input-str start)
        next-char (get-next-char input-str start to-add)]
     [first-char next-char]))

(defn convert-to-int
  ;; takes a pair of characters and converts them to integers
  [pair]
  (map #(read-string %) pair))

(defn count-pairs
  [input-str look-ahead]
  (let [index-range (range (count input-str))]
    (->> (map #(get-string-pair input-str % look-ahead) index-range)
         (map #(convert-to-int %))
         (filter #(= (first %) (second %)))
         (map first)
         (reduce +))))

(defn puzzle-1-1
  [input-str]
  (count-pairs input-str 1))

(defn puzzle-1-2
  [input-str]
  (count-pairs input-str (/ (count input-str) 2)))
