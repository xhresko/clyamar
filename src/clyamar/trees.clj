;;;; Oblivious regression trees implementation
(ns clyamar.trees
  (:require [clojure.core.reducers :as r]))

(defn mean [coll]
  (if (seq coll)
    (double (/ (r/fold + coll) (count coll)))))

(defn pow [x n]
  (reduce * (repeat n x)))

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

    (vector
    ;(map #(% 0)
         (filter #(< thold ((% 1) feature))
        (map vector (node 0) (node 1)))
      ;)

    ;(map #(% 0)
         (filter #(>= thold ((% 1) feature))
        (map vector (node 0) (node 1)))))
;)

(defn find-oblivious-split [nodes thresholds]
    ;; NODES - list of pairs (targets, features)
  (reduce (fn [x y] (if (< (x 2) (y 2)) x y) ) ; select minimal Cicha variance
   (map (fn [th] (let [[feature thold] th]
                    (vector feature thold
                           (cicha (map #(targets-split feature thold %) nodes))))) thresholds)))

(defn cicha [bin-targets]
(m-variance (flatten (map #(repeat (count %) (mean %))
                (reduce into [] bin-targets)))))



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
;(f th features targets )


(targets-split 0 2 [targets features])



(defn rank-splits [feature-vecs targets thresholds]
    ( let [overall-sum (r/fold + targets) overall-count (count targets)]
      ;; expects sorted feature vectors (by feature value)
      ;; expects sorted threshold values for each feature
      ;; targets as complete list of targets (?)
      ;; TODO: check if it cold be faster with map
      ;; return score for each split in map (FNUM, TH) -> score
      ()
      )
  )

(defn sum-score [func bins means]
  "Summarization of given metric over data.
  Used in MART - http://research-srv.microsoft.com/pubs/132652/MSR-TR-2010-82.pdf"
  (reduce + (map func bins means)))

(defn m-variance [data]
  "MART split variance
  Friedman (1999) - Greedy function approximation: A gradient boosting machine
  It supports continuous labels."
  (if (empty? data)
    nil
    (let [average (mean data)]
    (double (/ (reduce + (map #(pow (- average %) 2) data)) (count data))))
    ))

(defn rate-split [bins means]
  "Rate given split"
)

"
(defn rate-split [features targets feature thold]
  \"Rate given split\"
  (let [feat-vec (get-feat-vec features feature)
        ftar (map vector feat-vec targets)
        left (filter #(<= (get % 0) thold) ftar)
        right (filter #(> (get % 0) thold) ftar)]
    (if (or (empty? left) (empty? right))
      -1
      (sum-score m-variance
                 (vector
                   (map second
                        left)
                   (map second
                        right))))))

(defn split-node [features
                  targets
                  depth
                  tresholds
                  alpha]
  (let [[rating feature thold] (get-split features targets granularity)
        [l-node l-targets r-node r-targets] (apply-split features targets feature thold)]
    (cond
      (zero? depth)
      (* alpha (mean targets))
      (or (zero? (count l-targets))
          (zero? (count r-targets)))
      (* alpha (mean targets))
      :else (vector feature thold
                    (split-node l-node l-targets (dec depth) granularity alpha)
                    (split-node r-node r-targets (dec depth) granularity alpha)))))
"
