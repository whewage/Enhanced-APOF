(ns ppdsp.scratchpad
  (:require [ppdsp.classifier.base
             :refer [process-record describe-model]]
            [ppdsp.classifier.moa-classifier
             :refer [hoeffding-tree]]
            [ppdsp.trees.parse-model
             :refer [parse-moa-tree-model-string]])
  )

(let [demo-schema [{:name "input" :options :numeric}
                   {:name "output" :options ["a" "b"]}]
      demo-record [{:values [1 0] :id 1 } {:values [0 0] :id 2}]
	  
      model (hoeffding-tree demo-schema)
      ]
  ;; Process at least one record to initalise the tree.
  
  (for  [record demo-record]
     [ (process-record model record)
     ;; Print out MOA's representation of the tree.
         (println (describe-model model) "Model") ]
     
    )
  
    ;; Parse MOA's string into a tree representation.
  ;;(println (parse-moa-tree-model-string (describe-model model)))
 )

