(ns advent-clojure.day13
  (:require [clojure.string :as str]))

(defn parse-line
  [line]
  (let [splits (str/split line #":")
        scan-depth (read-string (first splits))
        scan-range (read-string (second splits))]
    {:depth scan-depth
     :range scan-range
     :direction 1}))

(defn parse-input
  [input]
  (->> input
       (str/split-lines)
       (reduce (fn [a v]
                 (let [parsed (parse-line v)]
                   (assoc a
                          (:depth parsed)
                          (assoc parsed :pos 0))))
               {})))

(defn step-scanner
  [scanner]
  (let [cur-dir (:direction scanner)
        next-pos (+ (:pos scanner) cur-dir)
        new-dir (if (or (< next-pos 0)
                        (>= next-pos (:range scanner)))
                  (* -1 cur-dir)
                  cur-dir)
        next-pos (+ (:pos scanner) new-dir)]
    (-> scanner
        (assoc :pos next-pos)
        (assoc :direction new-dir))))

(defn step-all-scanners
  [scanners]
  (reduce-kv (fn [m k v] (assoc m k (step-scanner v)))
             {}
             scanners))

(defn get-scanner-at-0
  [index scanners]
  (let [scanner (get scanners index)]
    (when (and scanner
               (= (:pos scanner) 0))
      scanner)))
    
(defn check-hit
  [cursor scanners]
  (if-let [scanner (get-scanner-at-0 (:pos cursor) scanners)]
    (let [severity (* (:range scanner) (:depth scanner))]
      (-> cursor
          (update :severity + severity)
          (assoc :got-hit true)))
    cursor))

(defn walk
  [cursor scanners]
  (let [cursor (-> (update cursor :pos inc)
                   (check-hit scanners))
        scanners (step-all-scanners scanners)
        last-depth (apply max (keys scanners))]
    (if (>= (:pos cursor) last-depth)
      cursor
      (recur cursor scanners))))

(defn check-math
  [scanners delay]
  (let [ranges (map #(:range (second %)) scanners)
        depths (map #(:depth (second %)) scanners)
        mod-amts (map #(- (* % 2) 2) ranges)
        maths (map #(mod (+ delay %2) %1) mod-amts depths)
        zero-pos (.indexOf maths 0)]
    (if (< zero-pos 0)
      delay
      (recur scanners (+ delay 1)))))

(defn puzzle-13-2
  [input]
  (let [scanners (parse-input input)]
    (check-math scanners 0)))

(defn puzzle-13-1
  [input]
  (let [scanners (parse-input input)
        cursor {:pos -1 :severity 0}]
    (walk cursor scanners)))
      
                
                    
