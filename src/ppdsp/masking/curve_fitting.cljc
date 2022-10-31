;;(ns ppdsp.masking.curve_fitting
"This is a standalone module. Curve fitting module cannot be run in notebooks as JVM doesnot support most of the graphical libraries.
So this complete code needs to be run in the cmd and the plots will be saved to the respective working directory"
  (require '[cljplot.build :as b]
         '[cljplot.core :refer :all]
         '[fastmath.core :as m]
         '[fastmath.kernel :as k]
         '[fastmath.random :as r]
         '[fastmath.regression :as reg]
         '[clojure2d.color :as c]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.data.csv :as csv]
         '[incanter.core :as ic]
         '[incanter.datasets :as id]
         '[incanter.io :as ii]
    )



(defn draw-gp-for-BP
  "Draw multi function chart with legend.
  * xs - privacy values
  * ys - AEL values
  * max-xs min-xs - min max x values to define the domain
  * title - chart title
  * legend-name - legend title
  * labels - legend labels
  * gps - map of gaussian processes/regression to visualize
  * h, w - optional height and width of the chart"
  [xs ys max-xs min-xs title legend-name labels gps & [h w]]
  (let [pal (c/palette-presets :category10) ;; use category10 palette
        leg (map #(vector :line %1 {:color %2}) labels pal) ;; construct legend
        scatter-data (map vector xs ys)] ;; prepare scatter data
    (-> (xy-chart {:width (or w 600) :height (or h 300)}
                  (-> (b/series [:grid]
                                [:scatter scatter-data {:size 10}])
                      (b/add-multi :function gps {:samples 100
                                                  :domain [(- min-xs 0.01) (+ max-xs 0.01)]} {:color pal})) ;;[0.005 0.04]-->arem-bp, [0.014 0.028]-->breast-cancer-bp [0.0 0.02]-->electricity
                  (b/add-axes :bottom)
                  (b/add-axes :left)
                  (b/add-legend legend-name leg)
                  (b/add-label :top title)
         )
     )
   )
 )

(defn draw-gp-for-AUC [xs ys max-xs min-xs title legend-name labels gps & [h w]]
  (let [pal (c/palette-presets :category10) ;; use category10 palette
        leg (map #(vector :line %1 {:color %2}) labels pal) ;; construct legend
        scatter-data (map vector xs ys)] ;; prepare scatter data
    (-> (xy-chart {:width (or w 600) :height (or h 300)}
                  (-> (b/series [:grid]
                                [:scatter scatter-data {:size 12}])
                      (b/add-multi :function gps {:samples 100
                                                  :domain [(- min-xs 0.01) (+ max-xs 0.01)]} {:color pal})) 
                  (b/add-axes :bottom)
                  (b/add-axes :left)
                  (b/add-legend legend-name leg)
                  (b/add-label :top title)
         )
     )
   )
 )
;;;
(defn normalize-results [x-auc] 
   "Normalizing the cumulative AUC values so it can be represented as a percentage of noise variance added 
    in the experimented noise addition rate values "
   (let [max-x-auc (apply max x-auc)
         min-x-auc (apply min x-auc)
         x-auc-list (apply list x-auc)
       ;  min-max []
        ]
       (loop [i 0 min-max [] ]
          (if (< i (count x-auc-list)) ;;(ncol y-auc)
             (recur (inc i) 
                     (conj min-max (double (/(- (nth x-auc-list i) min-x-auc)(- max-x-auc min-x-auc))))
              )
           min-max )
       )
       
    )
   
)
(def dataset-labels ["arem" "electricity" "breast-cancer" "nyc-taxi"])
;;;;;;
(for [label dataset-labels]
		(let [ ;current-dir (str "C:/Users/whewage/Documents/Waruni/PhD_Reaserch/ResearchQuestion2/ppdsp-master-tradeoff-V11-data-fitting-Gaussian-kernel/notebooks/workspace/arem/")
		        current-dir (str "notebooks/workspace/"label) ;;workspace folder should be changed according to the dataset
		       ael-file  (str current-dir "/masked-AEL.csv")
		       bp-file  (str current-dir "/final-breach-probabilities.csv")
		       auc-file (str current-dir "/logistic-cumulative-privacy-auc.csv")
		       ael-data (ic/to-matrix (ii/read-dataset ael-file))
					 bp-data (ic/to-matrix (ii/read-dataset bp-file))
					 auc-data (ic/to-matrix (ii/read-dataset auc-file))
		       k-vals (ic/sel ael-data :cols 0) ;k-values
		       x-bp (ic/sel bp-data :cols 1) ;bp values
		       x-auc (ic/sel auc-data :cols 4)
		       norm-x-auc (normalize-results x-auc)
		       y-ael (ic/sel ael-data :cols 1)
		       x-auc-max (apply max norm-x-auc)
		       x-auc-min (apply min norm-x-auc)
		       x-bp-max (apply max x-bp)
		       x-bp-min(apply min x-bp)
		       lambdas-bp [0.0001 0.001 0.01 0.1]  
		       lambdas-auc [0.0001 0.001 0.01 0.1]
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Plots for BP and AEL  
		       plot-gaussian-bp (save (draw-gp-for-BP x-bp y-ael x-bp-max x-bp-min "Regression with Gaussian kernel - BP" "Lambda" lambdas-bp 
		                                    (map vector lambdas-bp  (map #(reg/gaussian-process {:kernel (k/kernel :gaussian 0.01)  :lambda %} x-bp y-ael) lambdas-bp))         
		                               )  (str current-dir "/curve-fitting/Gaussian-bp-ael.jpg" )    ;;scaling parameter should be optimized
		                         )
		       plot-rational-quadratic-bp (save (draw-gp-for-BP x-bp y-ael x-bp-max x-bp-min "Regression with Rational-quadratic kernel - BP" "Lambda" lambdas-bp 
		                                    (map vector lambdas-bp  (map #(reg/gaussian-process {:kernel (k/kernel  :rational-quadratic 0.01)  :lambda %} x-bp y-ael) lambdas-bp))         
		                               )  (str current-dir"/curve-fitting/Rational-quadratic-bp-ael.jpg")
		                         )
		       plot-laplacian-bp (save (draw-gp-for-BP x-bp y-ael x-bp-max x-bp-min "Regression with Laplacian kernel - BP" "Lambda" lambdas-bp 
		                                    (map vector lambdas-bp  (map #(reg/gaussian-process {:kernel (k/kernel  :laplacian 0.01)  :lambda %} x-bp y-ael) lambdas-bp))         
		                               )  (str current-dir"/curve-fitting/Laplacian-bp-ael.jpg")
		                         )
		       plot-wave-bp (save (draw-gp-for-BP x-bp y-ael x-bp-max x-bp-min "Regression with Wave kernel - BP" "Lambda" lambdas-bp 
		                                    (map vector lambdas-bp  (map #(reg/gaussian-process {:kernel (k/kernel :wave 0.01)  :lambda %} x-bp y-ael) lambdas-bp))         
		                               ) (str current-dir"/curve-fitting/Wave-bp-ael.jpg")
		                         )
		       plot-mattern52-bp (save (draw-gp-for-BP x-bp y-ael x-bp-max x-bp-min "Regression with Mattern-52 kernel - BP" "Lambda" lambdas-bp 
		                                    (map vector lambdas-bp  (map #(reg/gaussian-process {:kernel (k/kernel :mattern-52 0.01)  :lambda %} x-bp y-ael) lambdas-bp))         
		                               )  (str current-dir"/curve-fitting/Mattern52-bp-ael.jpg")
		                         )
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Plots for AUC and AEL       
		      plot-gaussian-auc (save (draw-gp-for-AUC norm-x-auc y-ael x-auc-max x-auc-min "Regression with Gaussian kernel - AUC" "Lambda" lambdas-auc 
		                                    (map vector lambdas-auc  (map #(reg/gaussian-process {:kernel (k/kernel :gaussian 1)  :lambda %} norm-x-auc y-ael) lambdas-auc))         
		                               ) (str current-dir"/curve-fitting/Gaussian-auc-ael.jpg")
		                         )
		       plot-rational-quadratic-auc (save (draw-gp-for-AUC norm-x-auc y-ael x-auc-max x-auc-min "Regression with Rational-quadratic kernel - AUC" "Lambda" lambdas-auc 
		                                    (map vector lambdas-auc  (map #(reg/gaussian-process {:kernel (k/kernel  :rational-quadratic 1)  :lambda %} norm-x-auc y-ael) lambdas-auc))         
		                               ) (str current-dir"/curve-fitting/Rational-quadratic-auc-ael.jpg")
		                         )
		       plot-laplacian-auc (save (draw-gp-for-AUC norm-x-auc y-ael x-auc-max x-auc-min "Regression with Laplacian kernel - AUC" "Lambda" lambdas-auc 
		                                    (map vector lambdas-auc  (map #(reg/gaussian-process {:kernel (k/kernel  :laplacian 1)  :lambda %} norm-x-auc y-ael) lambdas-auc))         
		                               )  (str current-dir"/curve-fitting/Laplacian-auc-ael.jpg")
		                         )
		       plot-wave-auc (save (draw-gp-for-AUC norm-x-auc y-ael x-auc-max x-auc-min "Regression with Wave kernel - AUC" "Lambda" lambdas-auc 
		                                    (map vector lambdas-auc  (map #(reg/gaussian-process {:kernel (k/kernel :wave 1)  :lambda %} norm-x-auc y-ael) lambdas-auc))          
		                               ) (str current-dir"/curve-fitting/Wave-auc-ael.jpg")
		                         )
		       plot-mattern52-auc (save (draw-gp-for-AUC norm-x-auc y-ael x-auc-max x-auc-min "Regression with Mattern-52 kernel - AUC" "Lambda" lambdas-auc 
		                                    (map vector lambdas-auc  (map #(reg/gaussian-process {:kernel (k/kernel :mattern-52 1)  :lambda %} norm-x-auc y-ael) lambdas-auc))         ;
		                               )  (str current-dir "/curve-fitting/Mattern52-auc-ael.jpg")
		                         )
		       
		      ] 
		  
		)
)
