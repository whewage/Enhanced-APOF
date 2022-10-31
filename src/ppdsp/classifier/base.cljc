(ns ppdsp.classifier.base
  (:require [clojure.edn :as edn];;;;;;;waruni
            [clojure.java.io :as io]
            );;;;;waruni
  )

(defprotocol Classifier
  (process-record [classifier record]
    "Returns a class/confidence pair for the given record, and then
    trains the classifier on the record.")
  (describe-model [classifier]
    "Returns a representation of the model learned by the classifier.")
  )
