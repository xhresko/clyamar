;;;; Oblivious regression forest implementation
(ns clyamar.trees
  (:require [clojure.core.reducers :as r]))

(defn mean [coll]
  (if (seq coll)
    (double (/ (r/fold + coll) (count coll)))))

(defn pow [x n]
  (reduce * (repeat n x)))

(defn mse [labels predictions]
  "Calculate Mean Square Error."
  (float (/ (reduce + (map #(pow (- %1 %2) 2) labels predictions)) (count labels))))


(defn m-variance [data]
  "MART split variance
  Friedman (1999) - Greedy function approximation: A gradient boosting machine
  It supports continuous labels."
  (if (empty? data)
    nil
    (let [average (mean data)]
      (double (/ (reduce + (map #(pow (- average %) 2) data)) (count data))))))


(defn create-thresholds [features granularity]
  "Create set of tresholds for each feature."
  (reduce into [] (map (fn [n coll] (map #(vector n %) coll)) (range) ;; into pairs (f, v)
                       (apply map
                              #(let [uniqe (sort (set %&))]
                                (take granularity           ;; drop the last value if it is present
                                      (drop 1               ;; remove lowest margin
                                            (take-nth
                                              (quot
                                                (count uniqe)
                                                (min (inc granularity) (count uniqe)))
                                              uniqe))))
                              features))))


(defn targets-split [feature thold node]
  "Create split from given node and feature + treshold.
    Returns sequence of sequences of targets."
  (vector
    (map #(% 0)
         (filter #(< thold ((% 1) feature))
                 node))
    (map #(% 0)
         (filter #(>= thold ((% 1) feature))
                 node))))

(defn cicha-score [bin-targets]
  " Calculate Cicha score for given sequences of targets."
  (let [bins (reduce into [] bin-targets)]
    (if (some empty? bins)
      0
      (m-variance (flatten (map #(repeat (count %) (mean %))
                                bins))))))

(defn neco-score [bin-targets]
  "Calculate Neco score for given sequences of targets.
  Resulting ordering should be equivalent with Cicha score."
  (let [bins (reduce into [] bin-targets)]
    (r/fold + (map (fn [x] (if
                             (empty? x)
                             0
                             (/ (pow (r/fold + x) 2) (count x))))
                   bins))))


(defn find-oblivious-split [nodes thresholds]
  "Maximize metric (neco-score) over possible thresholds."
  ;; NODES - list of pairs (targets, features)
  (reduce (fn [x y] (if (and (not= 0 x) (>= (x 2) (y 2))) x y)) [0 0 -1] ; select maximal Neco-score
          (filter #(< 0 (% 2))
                  (map (fn [th] (let [[feature thold] th]
                                  (vector feature thold
                                          (neco-score (map #(targets-split feature thold %) nodes))))) thresholds))))

(defn split-one-node [feature thold node]
  "Create two new nodes from previous one
  using given feature index and threshold value"
  (vector
    (filter #(>= thold ((% 1) feature))
            node)
    (filter #(< thold ((% 1) feature))
            node)))


(defn apply-split [feature thold nodes]
  "Apply given feature/threshold on the collection of nodes."
  (reduce into []
          (map #(split-one-node feature thold %)
               nodes)))

(defn leaf-score [node alpha]
  "Calculate prediction for given leaf node.
  Using mean score of sample multiplied by learning rate."
  (if (seq node)
    (* alpha (/ (r/fold + (map #(% 0) node)) (count node)))
    0))

(defn leaves-score [nodes alpha]
  "Calculate predictions for all leaves in collection."
  (apply vector (map #(leaf-score % alpha) nodes)))


(defn split-tree-node [nodes thresholds depth alpha]
  "Node builder function. Find and create split.
  For the last level return score for all leaves."
  (let [[feature thold _] (find-oblivious-split nodes thresholds)
        level (dec depth)
        new-nodes (apply-split feature thold nodes)]
    (if (> 0 level)
      (vector (leaves-score nodes alpha))
      (cons (vector feature thold) (split-tree-node new-nodes thresholds level alpha)))))

(defn create-tree [targets features depth granularity alpha]
  "Create one oblivious tree."
  (let [nodes [(map vector targets features)]
        thresholds (create-thresholds features granularity)]
    (split-tree-node nodes thresholds depth alpha)))

(defn tree-rank [sample tree]
  "Use given tree to obtain rank for given sample."
  (let [checks (reverse (drop-last tree))]
    ((last tree)
      (r/fold +
              (map (fn [rule coef]
                     (if (> (sample (rule 0)) (rule 1))
                       (pow 2 coef)
                       0)
                     ) checks (range))))))

(defn forest-rank [forest sample]
  "Use given forest (tree coll) to obtain rank for given sample."
  (r/fold + (map #(tree-rank sample %) forest)))


(defn create-forest [labels features depth granularity alpha trees]
  "Create forest of oblivious trees."
  (loop [tree-num 1
         targets labels
         forest []]
    (if (> tree-num trees)
      forest
      (let [one-tree (create-tree targets features depth granularity alpha)
            next-num (inc tree-num)
            next-targets (map - targets (map #(tree-rank % one-tree) features))]
        (println "Tree no." tree-num)
        (println one-tree)
        (println "MSE:")
        (println (mse labels (map #(forest-rank forest %) features)))
        (recur next-num next-targets (conj forest one-tree))))))
