(ns clyamar.core
  (:gen-class)
  (:require [clyamar.lightsvm]
            [clyamar.trees]))

  (defn -main
        "I don't do a whole lot ... yet."
        [& args]
        (println "Args:" args)
        (let [[train-file] args
              features (clyamar.lightsvm/read-features train-file)
              labels (clyamar.lightsvm/read-labels train-file)
              tholds (clyamar.trees/create-thresholds features 10)]
          (println (count labels) (count (first features)) train-file)))
