;;;; Reader for datasets saved in LightSVM/LibSVM format.
(ns clyamar.lightsvm)

(use 'clojure.java.io)
(require '[clojure.string :as str])


(defn parse-label [line]
  (read-string ((str/split line #" ") 0)))

(defn parse-features [line]
  (into [] (map
             #(read-string ((str/split % #":") 1))
             ; Drop the "first" feature (query id in datasets)
             ;(drop 2 (str/split ((str/split line #"#") 0) #" ")))))
             (drop 1 (str/split ((str/split line #"#") 0) #" ")))))

(defn read-labels [svm-file]
  "Read numerical labels from LightSVM file."
  (map parse-label
       (str/split-lines
         (slurp svm-file))))

(defn read-features [svm-file]
  "Read features from LightSVM file."
  ; TODO: Remember correct numbering of features
  (map parse-features
       (str/split-lines
         (slurp svm-file))))
