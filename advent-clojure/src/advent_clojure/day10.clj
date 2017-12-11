(ns advent-clojure.day10
  (:require [clojure.string :as str]))

(def real-rope (vec (range 0 256)))

(defn add-within
  [rope & nums-to-add]
  (mod (apply + nums-to-add) (count rope)))

(defn cut-and-tie
  [rope at-indx]
  (vec (concat (subvec rope at-indx)
               (take at-indx rope))))
  
(defn twist
  ([rope lengths]
   (twist rope lengths 0 0))
  ([rope lengths skip-size offset]
   (let [length (first lengths)
         lengths (rest lengths)
         new-start (add-within rope skip-size length)
         new-rope (->> (reverse (take length rope))
                       (#(concat % (subvec rope length)))
                       (vec)
                       (#(cut-and-tie % new-start)))
         new-offset (add-within rope
                                offset
                                (- (count rope)
                                   (+ skip-size length)))
         new-skip-size (+ 1 skip-size)]
     (if (<= (count lengths) 0)
       {:rope new-rope :offset new-offset :skip-size new-skip-size}
       (recur new-rope lengths new-skip-size new-offset)))))

(defn twist-n-times
  ([rope lengths n]
   (twist-n-times rope lengths n 0 0))
  ([rope lengths n skip-size offset]
   (let [answer (twist rope lengths skip-size offset)]
     (if (= n 1)
       answer
       (recur (:rope answer)
              lengths
              (- n 1)
              (:skip-size answer)
              (:offset answer))))))

;; hardcoded ending to the lengths as described by puzzle
(def HARD_END [17 31 73 47 23])

(defn puzzle-10-2
  [rope input]
  (let [lengths (concat (map int (char-array input)) HARD_END)]
    (->> (twist-n-times rope lengths 64)
         (#(cut-and-tie (:rope %) (:offset %)))
         (partition 16)
         (map #(apply bit-xor %))
         (map #(format "%x" %))
         (reduce str))))

(defn puzzle-10-1
  [rope input]
  (let [lengths (->> (str/split input #",")
                     (map read-string)
                     vec)
        answer (twist rope lengths)
        final-rope (cut-and-tie (:rope answer) (:offset answer))
        mult (* (first final-rope) (second final-rope))]
    [mult answer]))
    
     
