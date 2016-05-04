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
                 node
                 ))
       (map #(% 0)
         (filter #(>= thold ((% 1) feature))
                 node
                 ))))

(defn cicha-score [bin-targets]
  " Calculate Cicha score for given sequences of targets."
  (let [bins (reduce into [] bin-targets)]
  (if (some empty? bins)
    0
  (m-variance (flatten (map #(repeat (count %) (mean %))
                bins))))))

(defn neco-score [bin-targets]
  " Calculate Neco score for given sequences of targets.
  Resulting ordering should be equivalent with Cicha score."
  (let [bins (reduce into [] bin-targets)]
  (r/fold + (map (fn [x] (if
                 (empty? x)
                 0
                 (/ (pow (r/fold + x) 2) (count x))))
                 bins))))

;;; Everything in following code is highly unstable !!!


(defn find-oblivious-split [nodes thresholds]
    ;; NODES - list of pairs (targets, features)
  (reduce (fn [x y] (if (and (not= 0 x) (>= (x 2) (y 2))) x y)) [0 0 -1]; select minimal Cicha variance
  (filter #(< 0 (% 2))
    (map (fn [th] (let [[feature thold] th]
                    (vector feature thold
                           ;(cicha-score (map #(targets-split feature thold %) nodes))))) thresholds)
                           (neco-score (map #(targets-split feature thold %) nodes))))) thresholds)
          ))
  )

(defn split-one-node [feature thold node]
    (vector
         (filter #(>= thold ((% 1) feature))
                 node)
         (filter #(< thold ((% 1) feature))
                 node)))


(defn apply-split [feature thold nodes]
  (reduce into [] (map #(split-one-node feature thold %)
       nodes)))

(defn leaf-score [node alpha]
  (if (seq node)
   (* alpha (/ (r/fold + (map #(% 0) node)) (count node)))
    0
    ))

(defn leaves-score [nodes alpha]
  (apply vector (map #(leaf-score % alpha) nodes)))

(defn tf-to-node [targets features] (map vector targets features))



; (leaves-score [(tf-to-node targets-b features-b) (tf-to-node targets features)] 0.1)

(defn build-tree [nodes thresholds depth alpha]
  ;(println "Depth:" depth)
  ;(doseq [n nodes]
   ; (println n)
   ; )

  ( let [[feature thold score]  (find-oblivious-split nodes thresholds)
         level (dec depth)
         new-nodes (apply-split feature thold nodes)]
        (if (> 0 level)
        (vector (leaves-score nodes alpha))
        ;(vector feature thold (build-tree new-nodes thresholds level) )
          (cons (vector feature thold) (build-tree new-nodes thresholds level alpha) )
          )))

(defn tree [targets features depth granularity alpha]
   (let [nodes [(map vector targets features)]
        thresholds (create-thresholds features granularity)]
     ;(println thresholds)
     (build-tree nodes thresholds depth alpha)
     ))

(defn tree-rank [sample tree]
   (let [checks (reverse (drop-last tree))
         depth (count checks)]
     ((last tree)
     (r/fold +
     (map (fn [rule coef]
            (if  (> (sample (rule 0)) (rule 1))
              (pow 2 coef)
              0)
            ) checks (range))))))

(defn forest [labels features depth granularity alpha trees]
  (loop [tree-num 1
         targets labels
         forest []]
    (if (> tree-num trees)
     forest
    (let [one-tree (tree targets features depth granularity alpha)
          next-num (inc tree-num)
          next-targets (map - targets (map #(tree-rank % one-tree) features))]
      (println "Tree no." tree-num)
      (println one-tree )
            (recur next-num next-targets (conj forest one-tree))))))



;(map - [1 2 3] [1 1 1])

;(find-oblivious-split [(tf-to-node targets-b features-b)] (create-thresholds features-b 10))
;(tree targets-b features-b 2 15 1.0)
;(forest targets-b features-b 2 15 0.1 100)

; (apply-split 5 3 [(tf-to-node targets-b features-b)])

; (tree-rank
;   [1 1 1 1 1 5 4 1 5 5 1 4 1 7 1 8 9 9 1 10]
;   (tree targets-b features-b 10 15)
;   )

; ((last (tree targets-b features-b 2 15)) )

;(def features [[1 1 5 2 3 5 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
;               [2 1 4 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
;               [2 1 4 2 3 1 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
;               [2 1 4 2 3 1 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
 ;              [4 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
 ;              [4 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
 ;              [4 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10]])
;(def targets   [1 2 0 1 3 2 1] )
;
;(def features-b [[1 1 5 2 3 5 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
;                 [2 1 4 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
;                 [2 1 4 2 3 1 4 4 6 7 6 6 7 7 8 8 9 9 10 10]
;                 [4 1 2 2 3 3 4 4 5 5 6 2 1 7 8 8 9 9 10 10]
;                 [4 1 2 2 3 3 4 5 5 5 6 6 7 7 1 8 9 9 10 10]
;                 [1 1 5 2 3 5 4 4 5 5 6 4 7 7 8 8 9 9 10 10]
;                 [2 1 4 2 3 3 1 4 5 2 6 6 7 7 3 8 9 9 10 10]
;                 [2 1 4 2 3 1 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
;                 [2 1 4 2 3 1 4 4 5 5 6 6 2 7 3 8 9 9 10 10]
;                 [4 1 2 2 3 3 4 4 5 5 6 6 2 7 8 8 9 9 10 10]
;                 [4 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10]
;                 [4 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10]])
;(def targets-b [1 0 0 1 0 2 0 0 1 0 0 1 0] )
;
;(/ (+ 10 8 6) 3)

;(cicha-score [[[1 3] [1 2 0]] [[1 2] [1 3 0]]])
;(cicha-score [[[1 ] [2 1 3 0]] [[1] [1 3 0 2]]])
;(neco-score [[[1 ] [2 1 3 0]] [[1] [1 3 0 2]]])
;(neco-score [[[1 3] [1 2 0]] [[1 2] [1 3 0]]])

;;(map #(map first %)  (targets-split 0 1 [targets features]))
;(find-oblivious-split (map #(apply tf-to-node %) [[targets-b features-b]]) (create-thresholds features 1))


;(find-oblivious-split (map #(apply tf-to-node %) [ [targets features] [targets-b features-b]]) (create-thresholds features 1))
;(m-variance (flatten (map #(repeat (count %) (mean %) ) (map flatten [[1 0] [1 1 0] [[1 1] [1 1 0]]]))))

;(def nodes [ [targets features] [targets-b features-b]])
;(def thresholds (create-thresholds features 2))

; (if (some empty? [[] [2] [2] [1 2]]) 0 1)
