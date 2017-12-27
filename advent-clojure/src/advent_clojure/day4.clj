(ns advent-clojure.day4
  (:require [clojure.string :as str]))

(defn convert-input-to-tokens
  [input]
  (->> (str/split-lines input)
       (map #(str/split % #" "))))

(defn is-anagram?
  [x y]
  (and (= (count x) (count y))
       (every? true? (map #(str/includes? (str y) (str %)) x))
       (every? true? (map #(str/includes? (str x) (str %)) y))))

  
(defn has-duplicates?
  ;; assumes int-list is a list of ints in reverse order
  [str-list equality-fn]
  (let [this-str (first str-list)
        other-strs (rest str-list)
        sames (filter #(equality-fn this-str %) other-strs)]
    (if (empty? other-strs)
      false
      (if (empty? sames)
        (recur other-strs equality-fn)
        true))))

(defn puzzle-4-2
  [input]
  (->> (convert-input-to-tokens input)
       (map #(has-duplicates? % is-anagram?))
       (filter false?)
       (count)))

(defn puzzle-4-1
  [input]
  (->> (convert-input-to-tokens input)
       (map #(has-duplicates? % =))
       (filter false?)
       (count)))

  
