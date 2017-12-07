(ns advent-clojure.day3)

(defn doubles-seq
  ;; generates a list of numbers that appear twice then increment by q
  ;; e.g. (1 1 2 2 3 3 4 4 ... n n)
  ([]
   (doubles-seq 1))
  ([a]
   (lazy-seq (cons a (cons a (doubles-seq (+ a 1)))))))

(defn is-turn?
  [pos turn-list]
  (some #(= pos %) turn-list))

(defn get-turns
  [pos turn-list]
  (reduce (fn [a v]
            (let [last-turn (last a)]
              (if (>= last-turn pos)
                (reduced a)
                (conj a (+ last-turn v)))))
          [1] ; initial value
          turn-list)) ; infinite sequence of distances

(def DIR_SOUTH 0)
(def DIR_EAST 1)
(def DIR_NORTH 2)
(def DIR_WEST 3)
(def NUM_DIRECTIONS 4)

(defn get-next-location
  [direction prev-location]
  (cond
    (= DIR_SOUTH direction) (assoc prev-location :y (+ 1 (:y prev-location)))
    (= DIR_EAST direction) (assoc prev-location :x (+ 1 (:x prev-location)))
    (= DIR_NORTH direction) (assoc prev-location :y (+ -1 (:y prev-location)))
    (= DIR_WEST direction) (assoc prev-location :x (+ -1 (:x prev-location)))))

(defn turn
  [cur-direction]
  (mod (+ 1 cur-direction) NUM_DIRECTIONS))
    
(defn build-nodes
  [cur-index direction prev-location turn-list]
  (let [next-location (get-next-location direction prev-location)
        next-direction (if (is-turn? (+ 1 cur-index) turn-list)
                         (turn direction)
                         direction)]
    (lazy-seq (cons {:location prev-location}
                     (build-nodes (+ 1 cur-index) next-direction next-location turn-list)))))

;; how much do we add to the current location to find a neighbor?
(def NEIGHBORS [[0 -1] [-1 -1] [-1 0]
                [-1 1] [0 1] [1 1]
                [1 0] [1 -1]])

(defn get-neighbors
  ;; gets all nodes from the hashmap that border this location
  [node-hash location]
  (let [targets (map #(map + [(:x location) (:y location)] %) NEIGHBORS)
        neighbors (map #(get node-hash %) targets)]
    (remove nil? neighbors)))

(defn get-neighbor-sum
  ;; gets the tally of all values that border this location
  [location node-hash]
  (let [tally
        (->> location
             (get-neighbors node-hash)
             (map #(get % :value))
             (reduce +))]
    ;; default to 1 if we don't find any neighbors
    (if (> tally 0) tally 1)))

(defn walk-spiral-2
  [cur-index stop-val all-nodes node-hash]
  (let [cur-node (nth all-nodes cur-index)
        cur-key [(get-in cur-node [:location :x])
                 (get-in cur-node [:location :y])]
        neighbor-sum (get-neighbor-sum (:location cur-node) node-hash)
        ;; redefine cur-node with our new value
        cur-node (assoc cur-node :value neighbor-sum)
        ;; store it in a hashmap that is easily findable
        node-hash (assoc node-hash cur-key cur-node)] 
    (if (> neighbor-sum stop-val)
      cur-node
      (recur (+ cur-index 1) stop-val all-nodes node-hash))))
  
(defn puzzle-3-2
  [input]
  (let [all-nodes (build-nodes
                   1
                   DIR_EAST
                   {:x 0 :y 0}
                   (get-turns input (doubles-seq)))]
    (walk-spiral-2 0 input all-nodes nil)))

(defn puzzle-3-1-alt
  ;; This version walks the spiral with the build-nodes generator
  ;; but it is much slower than the math-y way below
  [input]
  (let [all-nodes (build-nodes
                   1
                   DIR_EAST
                   {:x 0 :y 0}
                   (get-turns input (doubles-seq)))
        final-node (nth all-nodes (- input 1))]
    (+ (Math/abs (get-in final-node [:location :x]))
       (Math/abs (get-in final-node [:location :y])))))

;;================================================
;; Puzzle 3-1 Math Version
;;================================================

(defn get-max-dist
  [side-len]
  (int (Math/floor (/ side-len 2))))

(defn walk-distance
  [index target-index start-dist direction min-dist max-dist]
  (if (= index target-index)
    start-dist
    (let [next-dist (+ start-dist direction)]
      (if (or (= next-dist min-dist)
              (= next-dist max-dist))
        (recur (+ index 1) target-index next-dist (* direction -1) min-dist max-dist)
        (recur (+ index 1) target-index next-dist direction min-dist max-dist)))))

(defn get-distance-to-center
  [source side-len spiral-range]
  (let [min-dist (get-max-dist side-len)
        max-dist (* min-dist 2)
        pos-in-spiral (- source (first spiral-range))
        direction -1
        start-dist (- max-dist 1)]
    (walk-distance 0 pos-in-spiral start-dist direction min-dist max-dist)))      

(defn get-spiral-range
  ([num-to-find start-index]
   (get-spiral-range num-to-find start-index 0))
  ([num-to-find start-index iteration]
   (let [side-len (+ 1 (* 2 iteration))
         range-len (max 1
                        (+ (* 2 (- side-len 2))
                           (* 2 side-len)))
         end-index (+ start-index range-len)]
     (if (and (>= num-to-find start-index)
              (<= num-to-find end-index))
       [side-len (range start-index end-index)]
       (recur num-to-find end-index (+ 1 iteration))))))

(defn puzzle-3-1
  ;; This version uses some math to build only the spiral ring
  ;; that contains the answer
  [input]
  (let [spiral-specs (get-spiral-range input 1)
        side-len (first spiral-specs)
        spiral-range (second spiral-specs)]
    (get-distance-to-center input side-len spiral-range)))
