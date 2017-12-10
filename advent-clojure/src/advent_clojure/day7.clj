(ns advent-clojure.day7
  (:require [clojure.string :as str]))

(defn get-children
  [line]
  (when-let [arrow-index (str/index-of line ">")]
    (->> (subs line (+ 1 arrow-index))
         (#(str/split % #","))
         (map str/trim)
         (vec))))

(defn get-name
  [line]
  (str/trim (subs line
                  0
                  (str/index-of line "("))))

(defn get-weight
  [line]
  (str/trim (subs line
                  (+ 1 (str/index-of line "("))
                  (str/index-of line ")"))))

(defn parse-line
  [line]
  (let [program-name (get-name line)]
    { program-name
     {:name program-name
      :weight (get-weight line)
      :children (get-children line)}}))

(defn parse-input
  [input]
  (let [lines (->> (str/split-lines input)
                   (map parse-line)
                   (into {}))]
    lines))

(defn contains-node-key
  [node-key list]
  (filter #(= node-key %) list))

(defn contains-child?
  [child-key node]
  (->> (get (second node) :children)
       (contains-node-key child-key)))

(defn get-top-parent
  [programs node]
  (let [node-key (first node)
        parents (->> (map #(contains-child? node-key %) programs)
                     (filter not-empty)
                     (first))]
    (if (nil? parents)
      node
      (recur programs (get programs parents)))))

(defn get-odd-ball
  [nodes]
  (let [groups (group-by :full-weight nodes)]
    (filter #(= (count %) 1) groups)))

(defn get-full-weight
  [programs node]
  (let [node-key (:name node)
        our-weight (read-string (:weight node))
        children-keys (get node :children)]
    (if (not children-keys)
      (merge node {:full-weight our-weight :child-weights 0})
      (let [children (map #(get programs %) children-keys)
            child-weight-maps (map #(get-full-weight programs %) children)
            child-weights (map #(get % :full-weight) child-weight-maps)
            full-weight (+ our-weight (reduce + child-weights))]
        (if (not (apply = child-weights))
          (let [odd-ball (get-odd-ball child-weight-maps)]
            (println (str "full-weight: " full-weight))
            (println (str "child-weights: " (vec child-weights)))
            (println (str "child-weight-maps: " (vec child-weight-maps)))
            (throw (Exception. (str "child-weight-maps:" (vec odd-ball)))))
          (merge node {:full-weight full-weight :child-weights child-weights}))))))

(defn find-unbalanced-node
  [programs node]
  (let [obj node
        our-weight (read-string (:weight obj))
        children-keys (get obj :children)]
    (if (not children-keys)
      {:full-weight our-weight :child-weights 0}
      (let [children (map #(get programs %) children-keys)
            child-weight-maps (map #(get-full-weight programs %) children)
            child-raw-weights (map #(get children :weight))
            child-unbalanced (filter
                              (fn [x]
                                (some #(= :problem-node %) x))
                              child-weight-maps)]
        (if (> (count child-unbalanced) 0)
          child-unbalanced
          (let [child-weights (map #(get % :full-weight) child-weight-maps)
                full-weight (+ our-weight (reduce + child-weights))]
            (if (not (apply = child-weights))
              (let [odd-ball (get-odd-ball child-weights)
                    _ (println (str "child-weights: " child-weights))
                    _ (println (str "odd-ball: " odd-ball))]
                {:full-weight full-weight :child-weights child-weights :problem-node true :children children})
              {:full-weight full-weight :child-weights child-weights})))))))


(defn puzzle-7-2
  [input]
  (let [programs (parse-input input)
        top-node (->> (map #(get-top-parent programs %) programs)
                      (remove nil?)
                      (first))]
    (try 
      (get-full-weight programs (second top-node))
      (catch Exception e (str "caught e " (.getMessage e))))))


(defn puzzle-7-1
  [input]
  (let [programs (parse-input input)]
    (->> (map #(get-top-parent programs %) programs)
         (remove nil?))))

