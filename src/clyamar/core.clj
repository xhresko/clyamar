(ns clyamar.core
  (:gen-class)
  (:require [clyamar.lightsvm]
            [clyamar.trees]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as str]))
(use 'clojure.java.io)

(def cli-options
  [;; First three strings describe a short-option, long-option with optional
   ;; example argument description, and a description. All three are optional
   ;; and positional.
   ["-t" "--trees N" "Learn N trees."
    :default 10
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 1 % 0x10000) "Must be a number between 1 and 65536"]]
   ["-d" "--depth N" "Set tree depth to N."
    :default 5
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 1 % 0x10000) "Must be a number between 1 and 65536"]]
   ["-g" "--granularity N" "Consider maximum of N splits for each feature."
    :default 10
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 1 % 0x10000) "Must be a number between 1 and 65536"]]
   ["-a" "--alpha F" "Set learning coefficient."
    :default 0.13
    :parse-fn #(Float/parseFloat %)
    :validate [#(<= 0.0 % 1.0) "Must be a number between 0 and 1"]]
   ["-i" "--input-file FILE" "File with samples (in LightSVM format)"
    :parse-fn #(str %)
    ;:validate [ #((.exists (as-file ((str/split % #" ") 1)))) "Must be a valid file path."]
    ]
   ["-m" "--model-file FILE" "File for model saving/loading."
    :parse-fn #(str %)
    ;:validate [ #((.exists (as-file ((str/split % #" ") 1)))) "Must be a valid file path."]
    ]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["CLYAMAR 0.1.0 - implementation of oblivious MART forest."
        ""
        "Usage: program-name [options] action"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  train    Train new model"
        "  eval     Use an existing model for evaluation"
        "  predict  Use an existing model for generating predictions"
        ""
        "Please refer to https://github.com/xhresko/clyamar for more information."]
       (str/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (str/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    ;; Handle help and error conditions
    (cond
      (:help options) (exit 0 (usage summary))
      (not= (count arguments) 1) (exit 1 (usage summary))
      errors (exit 1 (error-msg errors)))
    ;; Execute program with options
    (case (first arguments)
      "train" (let [
                    {input-file  :input-file
                     depth       :depth
                     granularity :granularity
                     alpha       :alpha
                     trees       :trees
                     model-file  :model-file} options

                    features (clyamar.lightsvm/read-features input-file)
                    labels (clyamar.lightsvm/read-labels input-file)
                    model (clyamar.trees/create-forest
                            labels
                            features
                            depth
                            granularity
                            alpha
                            trees)]
                (if (nil? model-file)
                  (println model)
                  (spit model-file (str model))))
      "eval" (let [{input-file :input-file
                    model-file :model-file} options
                   features (clyamar.lightsvm/read-features input-file)
                   labels (clyamar.lightsvm/read-labels input-file)]
               (clyamar.trees/print-eval-forest labels features model-file))
      "predict" (let [{input-file :input-file
                       model-file :model-file} options
                      features (clyamar.lightsvm/read-features input-file)]
                  (clyamar.trees/print-predictions features model-file))
      (exit 1 (usage summary)))))