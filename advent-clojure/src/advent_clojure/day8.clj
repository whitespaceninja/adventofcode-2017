(ns advent-clojure.day8
  (:require [clojure.string :as str]))

(defn parse-line
  [line]
  (let [pieces (str/split line #" ")]
    {:register (nth pieces 0)
     :operation (nth pieces 1)
     :operation-amt (read-string (nth pieces 2))
     :conditional (take-last 3 pieces)}))

(defn conditional?
  [acc conditional]
  (let [reg (or (get acc (nth conditional 0)) 0)
        equality (nth conditional 1)
        amt (read-string (nth conditional 2))]
    (case equality
          "<" (< reg amt)
          "<=" (<= reg amt)
          ">" (> reg amt)
          ">=" (>= reg amt)
          "==" (= reg amt)
          "!=" (not= reg amt))))

(defn get-op-value
  [cur-val inst]
  (let [op (:operation inst)
        op-amt (:operation-amt inst)
        op-fn (if (= op "inc") + -)]
    (op-fn cur-val op-amt)))        

(defn execute-line
  [acc inst]
  (let [reg (:register inst)
        register-val (or (get acc reg) 0)
        new-val (if (conditional? acc (:conditional inst))
                  (get-op-value register-val inst)
                  register-val)
        cur-max (or (get acc :max) 0)
        acc (assoc acc :max (max cur-max new-val))]
    (assoc acc reg new-val)))

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (map parse-line)))

(defn puzzle-8-2
  [input]
  (let [instructions (parse-input input)]
    (->> (reduce execute-line {} instructions)
         (:max))))

(defn puzzle-8-1
  [input]
  (let [instructions (parse-input input)]
    (->> (reduce execute-line {} instructions)
         (#(dissoc % :max))
         (#(apply max (vals %))))))
