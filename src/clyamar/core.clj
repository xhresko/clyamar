(ns clyamar.core
  (:gen-class)
  (:require [clyamar.lightsvm]
            [clyamar.trees]))

  (defn -main
        "I don't do a whole lot ... yet."
        [& args]
        (println "Args:" args)
        (let [[train-file depth granularity alpha trees] args
              features (clyamar.lightsvm/read-features train-file)
              labels (clyamar.lightsvm/read-labels train-file)
              ;tholds (clyamar.trees/create-thresholds features 10)
              ; labels features depth granularity alpha trees
              model (clyamar.trees/forest
                      labels
                      features
                      (read-string depth)
                      (read-string granularity)
                      (read-string alpha)
                      (read-string trees))]
          (println (count labels) (count (first features)) train-file)
          (println model)
          ))
