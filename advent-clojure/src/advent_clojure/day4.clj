(ns advent-clojure.day4
  (:require [clojure.string :as str]))

(defn convert-input-to-tokens
  [input]
  (->> (str/split input #"\n")
       (map #(str/split % #" "))))

(defn is-anagram?
  [x y]
  (if (not= (count x) (count y))
    false
    (and (every? true? (map #(str/includes? (str y) (str %)) x))
         (every? true? (map #(str/includes? (str x) (str %)) y)))))
  
(defn has-duplicates?
  ;; assumes int-list is a list of ints in reverse order
  [str-list index equality-fn]
  (let [num-count (count str-list)
        this-str (nth str-list index)
        other-strs (take-last (- num-count index 1) str-list)
        sames (filter #(equality-fn this-str %) other-strs)
        next-index (+ index 1)]
    (if (= next-index num-count)
      false
      (if (empty? sames)
        (recur str-list (+ 1 index) equality-fn)
        true))))

(defn puzzle-4-2
  [input]
  (->> (convert-input-to-tokens input)
       (map #(has-duplicates? % 0 is-anagram?))
       (filter false?)
       (count)))

(defn puzzle-4-1
  [input]
  (->> (convert-input-to-tokens input)
       (map #(has-duplicates? % 0 =))
       (filter false?)
       (count)))

  
