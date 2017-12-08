(ns advent-clojure.day6
  (:require [clojure.string :as str]))

(defn convert-input-to-ints
  [input]
  (->> (str/split input #"\t")
       (map read-string)))

(defn get-biggest-block
  [int-list]
  (apply max int-list))

(defn get-next-block
  [index blocks]
  (mod (+ index 1) (count blocks)))

(defn increment-block
  [index blocks]
  (assoc blocks index (+ 1 (nth blocks index))))

(defn redistribute
  ;; redistributes the num-to-redist starting at the index into the rest of the blocks
  [index num-to-redist blocks]
  (let [blocks (increment-block index blocks)
        num-to-redist (- num-to-redist 1)]
    (if (= num-to-redist 0)
      blocks
      (recur (get-next-block index blocks) num-to-redist blocks))))

(defn redistribute-biggest
  [prev-configs blocks]
  (let [biggest (get-biggest-block blocks)
        index (.indexOf blocks biggest)
        ;; update blocks
        blocks (assoc blocks index 0)
        blocks (redistribute (get-next-block index blocks) biggest blocks)
        index-of-prev (.indexOf prev-configs blocks)]
    (if (>= index-of-prev 0)
      ;; if we've already seen this one, good to go!
      (let [num-redists (count prev-configs)
            cycle-size (- num-redists index-of-prev)]
        {:num-redists num-redists :cycle-size cycle-size})
      ;; else add this config and try again...
      (recur (conj prev-configs blocks) blocks))))

(defn puzzle-6
  [input]
  (let [blocks (vec (convert-input-to-ints input))]
   (redistribute-biggest [blocks] blocks)))

