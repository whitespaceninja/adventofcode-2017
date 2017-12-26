(ns advent-clojure.day12
  (:require [clojure.string :as str]))

(defn parse-line
  [line]
  (let [program (first (str/split line #" "))
        connected-list (subs line (+ (str/index-of line ">") 1))
        connected (->> (str/split connected-list #",")
                       (map str/trim)
                       vec)]
    {:program program :connected connected}))

(defn parse-input
  [input]
  (->> input
       (str/split-lines)
       (reduce (fn [a v]
                 (let [parsed (parse-line v)]
                   (assoc a
                          (:program parsed)
                          (:connected parsed))))
               {})))

(defn in?
  [coll v]
  (some #(= v %) coll))

(defn all-in?
  [coll v-coll]
  (every? true? (map #(in? coll %) v-coll)))

(defn get-ins
  [coll v-coll]
  (map #(some #{%} coll) v-coll))

(defn get-not-ins
  [coll v-coll]
  (reduce (fn [a v]
            (if (not (in? coll v))
              (conj a v)
              a))
          []
          v-coll))

(defn walk
  [trail prev-node next-nodes all-nodes]
  (let [trail (conj trail prev-node)
        new-ones (get-not-ins trail next-nodes)]
    (if (empty? new-ones)
      trail
      (map #(walk trail % (get all-nodes %) all-nodes) new-ones))))

(defn get-group
  [start nodes]
  (let [next-nodes (get nodes start)]
    (->> (walk [] start next-nodes nodes)
         flatten
         distinct)))

(defn reduce-groups
  [start groups nodes]
  (let [next-group (get-group start nodes)
        groups (conj groups next-group)
        all-prev (flatten groups)
        remaining (filter #(not (in? all-prev (first %))) nodes)]
    (if (empty? remaining)
      groups
      (recur (first (first remaining)) groups nodes))))        
  
(defn puzzle-12-2
  [input]
  (let [nodes (parse-input input)
        all-groups (reduce-groups "0" [] nodes)]
    (count all-groups)))
        
(defn puzzle-12-1
  [input]
  (let [nodes (parse-input input)
        group (get-group "0" nodes)]
    (count group)))
  
