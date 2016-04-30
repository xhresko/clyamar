;;;; Oblivious regression trees implementation
(ns clyamar.trees
  (:require [clojure.core.reducers :as r]))

(defn mean [coll]
  (if (seq coll)
    (double (/ (r/fold + coll) (count coll)))))

(defn pow [x n]
  (reduce * (repeat n x)))


(defn m-variance [data]
  "MART split variance
  Friedman (1999) - Greedy function approximation: A gradient boosting machine
  It supports continuous labels."
  (if (empty? data)
    nil
    (let [average (mean data)]
    (double (/ (reduce + (map #(pow (- average %) 2) data)) (count data))))
    ))


(defn create-thresholds [features granularity]
  "Create set of tresholds for each feature."
  (reduce into [] (map (fn [n coll] (map #(vector n %) coll)) (range) ;; into pairs (f, v)
   (apply map
         #(let [uniqe (sort (set %&))]
           (take granularity ;; drop the last value if it is present
                 (drop 1     ;; remove lowest margin
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
         (map vector (node 0) (node 1))))
       (map #(% 0)
         (filter #(>= thold ((% 1) feature))
         (map vector (node 0) (node 1))))))

(defn cicha-score [bin-targets]
  " Calculate Cicha score for given sequences of targets."
  (m-variance (flatten (map #(repeat (count %) (mean %))
                (reduce into [] bin-targets)))))

;;; Everything in following code is highly unstable !!!

(defn find-oblivious-split [nodes thresholds]
    ;; NODES - list of pairs (targets, features)
  (reduce (fn [x y] (if (< (x 2) (y 2)) x y) ) ; select minimal Cicha variance
   (map (fn [th] (let [[feature thold] th]
                    (vector feature thold
                           (cicha-score (map #(targets-split feature thold %) nodes))))) thresholds)))


(defn split-one-node [feature thold node]
    (vector
         (filter #(< thold ((% 1) feature))
         (map vector (node 0) (node 1)))
         (filter #(>= thold ((% 1) feature))
         (map vector (node 0) (node 1)))))

(defn apply-split [feature thold nodes]
  (reduce into [] (map #(split-one-node feature thold %)
       nodes))
  )

(apply-split 0 1 [[targets features] [targets-b features-b]])

(def features [[1 1 5 2 3 5 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
               [2 1 4 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
               [2 1 4 2 3 1 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
               [2 1 4 2 3 1 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
               [4 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
               [4 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
               [4 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10]])
(def targets   [1 2 0 1 3 2 1] )

(def features-b [[1 1 5 2 3 5 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
               [2 1 4 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
               [2 1 4 2 3 1 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
               [4 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
               [4 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10]])
(def targets-b [1 3 0 1 2] )

(map vector targets features)


(cicha [[[1 3] [1 2 0]] [[1 2] [1 3 0]]])
(cicha [[[1 ] [2 1 3 0]] [[1] [1 3 0 2]]])


(map #(map first %)  (targets-split 0 1 [targets features]))


(find-oblivious-split [ [targets features] [targets-b features-b]] (create-thresholds features 1))
(m-variance (flatten (map #(repeat (count %) (mean %) ) (map flatten [[1 0] [1 1 0] [[1 1] [1 1 0]]]))))

(def nodes [ [targets features] [targets-b features-b]])
(def thresholds (create-thresholds features 2))
