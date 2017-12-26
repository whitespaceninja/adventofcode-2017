(ns advent-clojure.day24
  (:require [clojure.string :as str]))

(defn parse-line
  [line]
  (let [pin-labels (str/split line #"/")
        pins (map read-string pin-labels)]
    (vec pins)))
  
(defn parse-input
 [input]
  (let [lines (str/split-lines input)
        components (map parse-line lines)]
    components))

(defn get-components
  [num-pins components]
  (filter (fn [x] (some #{num-pins} x)) components))

(defn remove-comp
  [comp-to-remove components]
  (remove #(= comp-to-remove %) components))

(defn remove-comps
  [comps-to-remove components]
  (let [comp-to-remove (first comps-to-remove)
        others (rest comps-to-remove)
        components (remove #(= comp-to-remove %) components)]
    (if (empty? others)
      components
      (recur others components))))

(defn remove-one
  [components num]
  (let [[n m] (split-with (partial not= num) components)]
    (concat n (rest m))))

(defn build-bridge
  [prev-comp prev-val bridge components depth]
  (let [bridge (+ bridge (apply + prev-comp))
        components (remove-comp prev-comp components)
        other-pin-val (first (remove-one prev-comp prev-val))
        next-comps (get-components other-pin-val components)
        depth (+ 1 depth)]
    (if (empty? next-comps)
      {:strength bridge :length depth}
      (map #(build-bridge % other-pin-val bridge components depth) next-comps))))

(defn puzzle-24-1
  [input]
  (let [components (parse-input input)
        starting-blocks (get-components 0 components)
        other-blocks (remove-comps starting-blocks components)
        strongest (->> (pmap #(build-bridge % 0 0 other-blocks 0) starting-blocks)
                     flatten
                     (sort-by :strength)
                     last)]
    strongest))
        
(defn puzzle-24-2
  [input]
  (let [components (parse-input input)
        starting-blocks (get-components 0 components)
        other-blocks (remove-comps starting-blocks components)
        bridges (->> (pmap #(build-bridge % 0 0 other-blocks 0) starting-blocks)
                     flatten
                     (sort-by :length))
        longest (last bridges)
        strongest (->> bridges
                      (filter #(= (:length longest) (:length %)))
                      (sort-by :strength)
                      last)]
    strongest))
