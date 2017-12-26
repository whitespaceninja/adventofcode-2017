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

(defn get-bridge-strength
  [bridge]
  (reduce (fn [a v]
            (+ a (apply + v)))
          0
          bridge))

(defn build-bridge
  [prev-comp bridge components depth]
  (let [bridge (conj bridge prev-comp)
        components (remove-comp prev-comp components)
        next-comps (->> (map #(get-components % components) prev-comp)
                        (apply concat))
        depth (+ 1 depth)]
    (if (empty? next-comps)
      (get-bridge-strength bridge)
      (apply max (map #(build-bridge % bridge components depth) next-comps)))))

(defn puzzle-24-1
  [input]
  (let [components (parse-input input)
        starting-blocks (get-components 0 components)
        other-blocks (remove-comps starting-blocks components)
        bridges (pmap #(build-bridge % [] other-blocks 0) starting-blocks)]
    bridges))
;    (map get-biggest-strength bridges))) 
        
       
