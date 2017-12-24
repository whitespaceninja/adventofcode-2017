(ns advent-clojure.day20
  (:require [clojure.string :as str]))

(defn get-first-xyz
  [line start-index]
  (let [pos-start (str/index-of line "<" start-index)
        pos-end (str/index-of line ">" start-index)
        coord-str (subs line (+ 1 pos-start) pos-end)
        coords (str/split coord-str #",")]
    {:x (read-string (nth coords 0))
     :y (read-string (nth coords 1))
     :z (read-string (nth coords 2))}))

(defn get-acc-score
  [acc]
  (+ (Math/abs (:x acc))
     (Math/abs (:y acc))
     (Math/abs (:z acc))))

(defn parse-line
  [line]
  (let [pos (get-first-xyz line (str/index-of line "p"))
        vel (get-first-xyz line (str/index-of line "v"))
        acc (get-first-xyz line (str/index-of line "a"))
        acc-score (get-acc-score acc)]
    {:pos pos
     :vel vel
     :acc acc
     :acc-score acc-score}))

(defn add-xyz-s
  [particle key1 key2]
  (let [new-vals (map #(reduce + %)
                      (map vector
                           (vals (key1 particle))
                           (vals (key2 particle))))
        new-xyz (zipmap [:x :y :z] new-vals)]
    new-xyz))

(defn tick-vel
  [particle]
  (let [new-vel (add-xyz-s particle :vel :acc)]
    (assoc particle :vel new-vel)))

(defn tick-pos
  [particle]
  (let [new-vel (add-xyz-s particle :vel :pos)]
    (assoc particle :pos new-vel)))

(defn tick-particle
  [particle]
  (->> particle
       tick-vel
       tick-pos))

(defn pos-different?
  [p1 p2]
  (or (not= (get-in p1 [:pos :x]) (get-in p2 [:pos :x]))
      (not= (get-in p1 [:pos :y]) (get-in p2 [:pos :y]))
      (not= (get-in p1 [:pos :z]) (get-in p2 [:pos :z]))))

(defn remove-dupes
  [acc particles]
  (if (= 0 (count particles))
    acc
    (let [p1 (first particles)
          others (rest particles)
          any-same? (some #(= false %) (map #(pos-different? p1 %) others))
          diffs (filter #(pos-different? p1 %) others)]
      (if any-same?
        (recur acc diffs)
        (recur (conj acc p1) others)))))

(defn tick-all-particles
  [particles]
  (->> particles
       (map tick-particle)
       (remove-dupes [])))

(defn tick-a-bunch
  [particles n]
  (if (= n 0)
    particles
    (recur (doall (tick-all-particles particles)) (- n 1))))

(defn puzzle-20-2
  [input]
  (let [particles (->> (str/split-lines input)
                       (map parse-line))
        final-particles (tick-a-bunch particles 1000)]
    (count final-particles)))

(defn puzzle-20-1
  [input]
  (let [particles (->> (str/split-lines input)
                       (map parse-line))
        ;; the closest particle is the one with lowest acceleration
        min-particle (apply min-key :acc-score particles)
        index (.indexOf particles min-particle)]
    index))
