(ns advent-clojure.day22
  (:require [clojure.string :as str]))

(defn create-node
  [character pos]
  {:pos [(int (first pos))
         (int (second pos))]
   :state (if (= character \#)
            :infected
            :clean)})

(defn parse-line
  [y line]
  (let [num-chars (count line)
        x (- (Math/floor (/ num-chars 2)))
        positions (map vector (range x num-chars) (repeat num-chars y))]
    (map create-node (seq line) positions)))
    
(defn parse-input
  [input]
  (let [lines (str/split-lines input)
        height (count lines)
        y-start (- (Math/floor (/ height 2)))
        y-range (range y-start (+ y-start height))
        node-list (->> (map parse-line y-range lines)
                       (apply concat)
                       vec)]
    (reduce-kv (fn [m k v]
                 (assoc m (:pos v) v))
               {}
               node-list)))

(defn get-node
  [pos nodes]
  (if (contains? nodes pos)
    (get nodes pos)
    {:pos pos
     :state :clean}))

(defn toggle-node-state
  [cur-node]
  (let [state (:state cur-node)
        new-state (cond 
                    (= state :clean) :infected
                    (= state :infected) :clean)]
    (assoc cur-node :state new-state)))

(defn toggle-node-state-evolved
  [cur-node]
  (let [state (:state cur-node)
        new-state (cond 
                    (= state :clean) :weakened
                    (= state :weakened) :infected
                    (= state :infected) :flagged
                    (= state :flagged) :clean)]
    (assoc cur-node :state new-state)))

(defn lock-direction
  [dir]
  (cond
    (>= dir NUM_DIRECTIONS) (- dir NUM_DIRECTIONS)
    (< dir 0) (+ dir NUM_DIRECTIONS)
    :default dir))

(def TURN_AMT {:infected -1
               :clean 1
               :weakened 0
               :flagged 2})

(defn turn
  [turn-amt virus]
  (let [new-facing (->> (:facing virus)
                        (+ turn-amt)
                        lock-direction)]
    (assoc virus :facing new-facing)))
  
(defn walk
  [nodes virus n toggle-fn infect-ctr]
  (let [cur-pos (:pos virus)
        cur-node (get-node cur-pos nodes)
        new-virus (->> virus
                       (turn (get TURN_AMT (:state cur-node)))
                       move)
        new-node (toggle-fn cur-node)
        infect-ctr (if (= :infected (:state new-node))
                     (+ 1 infect-ctr)
                     infect-ctr)
        nodes (assoc nodes cur-pos new-node)]
    (if (= n 1)
      {:last-virus virus :num-infected infect-ctr}
      (recur nodes new-virus (- n 1) toggle-fn infect-ctr))))
    
(def DIR_SOUTH 0)
(def DIR_EAST 1)
(def DIR_NORTH 2)
(def DIR_WEST 3)
(def NUM_DIRECTIONS 4)

(defn move
  [virus]
  (let [direction (:facing virus)
        x (first (:pos virus))
        y (second (:pos virus))
        new-pos (cond
                  (= DIR_SOUTH direction) [x (+ y 1)]
                  (= DIR_EAST direction) [(+ 1 x) y]
                  (= DIR_NORTH direction) [x (- y 1)]
                  (= DIR_WEST direction) [(- x 1) y])]
    (assoc virus :pos new-pos)))

(defn puzzle-22-1
  [input]
  (let [nodes (parse-input input)
        virus {:pos [0 0] :facing DIR_NORTH}]
    (walk nodes virus 10000 toggle-node-state 0)))

(defn puzzle-22-2
  [input]
  (let [nodes (parse-input input)
        virus {:pos [0 0] :facing DIR_NORTH}]
    (walk nodes virus 10000000 toggle-node-state-evolved 0)))
