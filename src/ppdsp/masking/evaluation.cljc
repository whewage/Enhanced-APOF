(ns ppdsp.masking.evaluation
  (:require [ppdsp.masking.base :refer [mask-dataset get-masked-schema
                                       make-dataset-mask
                                       make-distributed-dataset-mask]]
            [ppdsp.masking.projection
             :refer [ make-random-projection-with-noise-and-translation-mask-factory-cycles]]        
            [ppdsp.masking.attack-data :refer [build-io-attack-data]]
            [ppdsp.masking.single-stage-cumulative-attack :refer [known-io-projection-and-cumulative-noise-map-attack-single-stage]]
            [ppdsp.masking.two-stage-cumulative-attack :refer [known-io-projection-and-cumulative-noise-map-attack-two-stage]]
            [ppdsp.masking.two-stage-independent-attack :refer [known-io-projection-and-independent-noise-map-attack-two-stage]]
            [ppdsp.classifier.moa-classifier
             :refer [hoeffding-tree perceptron]]
            [ppdsp.dataset.base :refer [get-schema dataset->matrix dataset->classes matrix->dataset
                                       first-record normalise-dataset dataset-record-count pop-record-and-rest]]
            [ppdsp.training :refer [train-classifier get-accuracy ]] ;;waruni get-accuracy-cycles
            [ppdsp.classifier.base
             :refer [process-record describe-model]]
            [ppdsp.trees.parse-model
             :refer [parse-moa-tree-model-string]] ;;waruni
            [ppdsp.utils :refer [map-vals mean median std-dev pmap-pool
                                debug try-times sync-println]]
            [ppdsp.utils.random :refer [seeded-rng next-derived-seed!]]
            [ppdsp.utils.matrices :refer [get-matrix-rows random-li-row-indexes!]]
            [ppdsp.utils.timing :refer [get-current-thread-time!]]
             [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.string :as string]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as ms]
            [clojure.math.numeric-tower :refer [sqrt round expt ceil]]))

;; Use vectorz Java implementation for faster matrix operations.
(m/set-current-implementation :vectorz)

;; == Accuracy Evaluation ==

(defn test-classification-accuracy [classifier-fn dataset]
  (let [results (train-classifier (classifier-fn (get-schema dataset)) dataset)
        ]
   
    
    {:accuracy (get-accuracy results)   
     :raw-results results}
  )   
)

;;Window-based accuracy evaluation for data streams
#_(defn test-classification-accuracy-window
   "The perturbed data set is divided into windows and the accuracy is evaluated only for the current window.
  This scenariao was implemented considering infinite/large data streams because accuracy of the past data may not be relevant 
  anymore. The existing classifier(HAT) is discarded the the end of the previous window and start building HAT from the
  scratch at the begining of the current window. The current window is consist of two sections. First section lets the HAT 
  to learn and to matured and in the second section accuracy is measured."
   [classifier-fn dataset] ;;dataset has passed as an object here
   (def learning-window-size 5000)
   (def measuring-window-size 5000)
   (def win_number (atom 1))
   (let [results (train-classifier (classifier-fn (get-schema dataset)) dataset) ;;This should be changed. Should pass a window at a time to build the tree.
         ]
 
   (loop [i 0 
          dataset dataset]
      (let [[records-to-window rest-dataset] (pop-record-and-rest dataset) 
           
           ]
       (when records-to-window
		       (if (=(-> records-to-window :id) (* @win_number (+ learning-window-size measuring-window-size) ))
		            (do  
                  (swap! win_number inc)
                 ;;(println "Record" records-to-window) ;;Returns the top record from the dataset with ID ;;{:id 0, :values []}
                 )
		        )
		      
		     (recur (+ i 1) rest-dataset)
      )
      
     )
    )
   {:accuracy (get-accuracy results)   
     :raw-results results}
   )
  #_(let [ [record-test rest-dataset] 
        results (train-classifier (classifier-fn (get-schema dataset)) dataset)
        ;;(map-vals #(test-classification-accuracy-window % masked-dataset) classifier-fns )
      ]
    (println "Record" record-test) ;;Returns the top record from the dataset with ID ;;{:id 0, :values []}
    ; (println "Dataset in training" dataset) ;;Object
    {:accuracy (get-accuracy results)   
     :raw-results results}
  )   
)

;;;Waruni
(defn save-data [filename data]
  (io/make-parents filename)
  (with-open [w (io/writer filename)]
    (binding [*out* w]
      (pr data)))
  data)

(defn save-data-append-newline [filename data]
  (io/make-parents filename)
  (with-open [w (io/writer filename :append true)]
    (binding [*out* w]
      (println data )))
  data)

(defn load-data
  [filename]
  (with-open [reader (java.io.PushbackReader. (io/reader filename))]
    (edn/read reader)))

;; Distance measures

(defn pairwise-row-euclidean-distances
  "Given m*n matrix x, return a m*m matrix where each value v_{ij} returns the Euclidean distance between rows x_i and x_j. Given the
  matrix is symmetric, only the lower-triangular matrix of the result is returned.
 This implementation is based off that of Numpy: https://scikit-learn.org/stable/modules/generated/sklearn.metrics.pairwise.euclidean_distances.html"
  [x]
  (let [xy (let [x (m/matrix x)]
             (m/mmul x (m/transpose x)))
        ;; Clone is required, as we will be mutating xy
        xy-diag (m/clone (m/diagonal xy))
        row-count (m/row-count xy)
        ;; The basis for the result is xy
        res xy]
    ;; Mutate each cell in the result matrix, this uses less memory
    ;; than creating matrices to add values from xy-diag.
    (doseq [i (range row-count)
            j (range row-count)]
      (if (< j i)
        ;; Transform each cell value as needed (see Numpy
        ;; implementation).
        (m/mset! res i j
                 (sqrt
                  (+ (* -2 (m/mget res i j))
                     (m/mget xy-diag i)
                     (m/mget xy-diag j))))
        ;; If j >= i, set the value to zero, as we are constructing a
        ;; lower-triangular matrix.
        (m/mset! res i j 0)))
    ;; Return the result matrix
    res))

(defn row-distance-error
  "Computes the sum-of-squared-error (SSE) between the pairwise-row-euclidean-distance matrices of the dataset and masked-dataset."
  [dataset masked-dataset]
  (let [orig-dists (-> dataset
                       dataset->matrix
                       pairwise-row-euclidean-distances)
        masked-dists (-> masked-dataset
                         dataset->matrix
                         pairwise-row-euclidean-distances)
        ;; The basis for the result is orig-dists
        dist-error orig-dists]
    ;; Use mutation to save memory.
    (m/sub! dist-error masked-dists)
    (m/mul! dist-error dist-error)
    (m/esum dist-error)))

(defn relative-error
  "Return the relative error between the two record vectors, as defined in Liu (2007)."
  [true-vector recovered-vector]
  (/ (m/length (m/sub recovered-vector true-vector))
     (m/length true-vector)
     )
 )

;; == Privacy Evaluation ==

(defn get-record-bounds
  "For a given number of records and a range (between 0 and 1), return the lower (inclusive) and upper (exclusive) bound record indexes for
  that range centered at the middle of the set of records."
  [record-count record-range range-position] ;;flat-record-length 
  (if (= 1 record-range)
    ;; Short-circuit for full range.
    [0 record-count] 
    (case range-position
      :middle (let [onesided-bound (Math/round (* record-count  (* record-range 0.5))) 
                    mid-record (quot record-count 2)] 
                ;; Return the start and end bounds either side of the middle.
                [(max 0 (- mid-record onesided-bound))
                 (min record-count (+ mid-record onesided-bound))]) 
      :start [0 (Math/round (* record-count record-range))])))  

(defn get-default-known-record-counts
  "Return 1, half number of masked features (rounded down for odd number), and one less than masked features (max possible)."
  [masked-feature-count]
  (->> [1 (quot masked-feature-count 2) (dec masked-feature-count)]
       ;; Remove duplicates.
       (distinct)
       ;; Ensure all of the computed counts are greater than zero.
       (filter #(> % 0))))

(defn generate-io-attack-data!
  "Use the rng to generate data for a known input/output attack (in particular, selecting the known and unknown records).
   Already implemented to perform attacks for each cycle and for the flat areas in previous versions. But cannot use when cycle size is randomly selected." 
  [rng input-matrix masked-matrix projection-sigma cumulative-noise-sigma known-record-count 
   known-record-range known-record-range-position window-no window-size full-record-count] 
   (let [   
           window-no window-no ;(for every window) 
           record-count window-size            ;;(m/row-count masked-matrix) 
           full-record-count full-record-count
           no-of-windows (int (ceil (/ full-record-count window-size ))) 
           [unknown-record-start unknown-record-end] (get-record-bounds record-count known-record-range known-record-range-position)    ;;gives [0 record-count]  ;;flat-record-length 
           [known-record-start known-record-end] (get-record-bounds full-record-count known-record-range known-record-range-position)
           ;; Create a derived rng so that the number of known records does not effect subsequent rng in the experiment.
            records-rng (seeded-rng (next-derived-seed! rng))
             unknown-record-index (if (= window-no (- no-of-windows 1)) ;;This is checked for the final window. Otherwise outofbound exception
                                      (first (random-li-row-indexes! records-rng input-matrix 1 
                                                                :start-index (- (* record-count (+ window-no 1)) unknown-record-end)
                                                                :end-index full-record-count  ;;
                                                    )
                                             )  
                                     (first (random-li-row-indexes! records-rng input-matrix 1 
                                                                :start-index (- (* record-count (+ window-no 1)) unknown-record-end)
                                                                :end-index ( - (* record-count (+ window-no 1)) unknown-record-start 1)
                                             )
                                       )
                                        
                                    )       
             
             known-record-indexes (random-li-row-indexes! records-rng input-matrix  known-record-count
                                                      :start-index known-record-start 
                                                      :end-index   known-record-end                  ;;(m/row-count masked-matrix )  ;;for the full record range
                                                      :init-li-row-indexes [unknown-record-index]
                                   )
             
         ; known-record-indexes (if (= window-no (- no-of-windows 1)) ;This is checked for the final window. Otherwise outofbound exception
                                  #_ (random-li-row-indexes! records-rng input-matrix  known-record-count
                                                            :start-index (- (* record-count (+ window-no 1)) known-record-end);known-record-start 
                                                             :end-index full-record-count   ;known-record-end                  ;;(m/row-count masked-matrix )  ;;for the full record range
                                                             :init-li-row-indexes [unknown-record-index]
                                   )
                                  #_  (random-li-row-indexes! records-rng input-matrix  known-record-count
                                                            :start-index (- (* record-count (+ window-no 1)) known-record-end);known-record-start 
                                                             :end-index ( - (* record-count (+ window-no 1)) known-record-start 1) ;known-record-end                  ;;(m/row-count masked-matrix )  ;;for the full record range
                                                             :init-li-row-indexes [unknown-record-index]
                                   )
                              ; )
          
          
          
          
         ]
     ;(println "records-rng" records-rng)
   ;(println "window-no" window-no)
   ; (println "start" (- (* record-count (+ window-no 1)) known-record-end))
    ; (println "end" ( - (* record-count (+ window-no 1)) known-record-start ))
    ;(println  unknown-record-index " unknown-record-index")
   ; (println  known-record-indexes " known-record-indexes")
  ; (println "number of windows" no-of-windows)
     (build-io-attack-data projection-sigma cumulative-noise-sigma ;;independent-sigma
                           input-matrix masked-matrix
                           known-record-indexes unknown-record-index)
     )
 )

(defn attack-strategy-map
  []
  {;; Single stage Cumulative Noise Attacks
   :a-rp {:attack-function known-io-projection-and-cumulative-noise-map-attack-single-stage
          :cumulative-noise? false
          :bias-input-prob? false
          :only-one-known? false}
   :a-rpcn-single-stage-unbalanced {:attack-function known-io-projection-and-cumulative-noise-map-attack-single-stage
                                    :cumulative-noise? nil ;; Chosen based on cumulative-noise-sigma.
                                    :bias-input-prob? false
                                    :only-one-known? false}
   :a-rpcn-single-stage {:attack-function known-io-projection-and-cumulative-noise-map-attack-single-stage
                         :cumulative-noise? nil ;; Chosen based on cumulative-noise-sigma.
                         :bias-input-prob? true
                         :only-one-known? false}
   :a-rpcn-1-single-stage-unbalanced {:attack-function known-io-projection-and-cumulative-noise-map-attack-single-stage
                                      :cumulative-noise? nil ;; Chosen based on cumulative-noise-sigma.
                                      :bias-input-prob? false
                                      :only-one-known? true}
   :a-rpcn-1-single-stage {:attack-function known-io-projection-and-cumulative-noise-map-attack-single-stage
                           :cumulative-noise? nil ;; Chosen based on cumulative-noise-sigma.
                           :bias-input-prob? true
                           :only-one-known? true}
   ;; Two stage Cumulative Noise Attacks
   :a-rpcn-unbalanced {:attack-function known-io-projection-and-cumulative-noise-map-attack-two-stage
                       :cumulative-noise? nil ;; Chosen based on cumulative-noise-sigma.
                       :bias-input-prob? false
                       :only-one-known? false}
   :a-rpcn {:attack-function known-io-projection-and-cumulative-noise-map-attack-two-stage
            :cumulative-noise? nil ;; Chosen based on cumulative-noise-sigma.
            :bias-input-prob? true
            :only-one-known? false}
   :a-rpcn-1-unbalanced {:attack-function known-io-projection-and-cumulative-noise-map-attack-two-stage
                         :cumulative-noise? nil ;; Chosen based on cumulative-noise-sigma.
                         :bias-input-prob? false
                         :only-one-known? true}
   :a-rpcn-1 {:attack-function known-io-projection-and-cumulative-noise-map-attack-two-stage
              :cumulative-noise? nil ;; Chosen based on cumulative-noise-sigma.
              :bias-input-prob? true
              :only-one-known? true}
   ;; Two Stage Independent Noise Attacks -removed
    
   })

(defn perform-attack
  "Perform an attack with the given io-attack-data with the given
  attack-strategies. Take the best of attempt-count attempts for each
  strategy. Return a map of strategies to the best recovery attempt
  with that strategy."
  [rng io-attack-data original-record attempt-count ;window-no
   & {:keys [optimization-max-evaluations optimization-relative-threshold
             attack-strategies]}]
  (let [;; Use a derived seed to have a different seed for each attack, but add the attempt index so that the same seeds are used for each strategy in this attack.
        base-attack-seed (max 0 (- (next-derived-seed! rng) attempt-count))
        attack-strategies (or attack-strategies (sort (keys (attack-strategy-map))))
        ;; If no cumulative noise, do not include attack strategies that account for cumulative noise.
       ;; attack-strategies (if (<= (:cumulative-noise-sigma io-attack-data) 0)
                           #_ (remove #(contains? #{:a-rpcn-single-stage-unbalanced
                                                    :a-rpcn-single-stage
                                                    :a-rpcn-1-single-stage-unbalanced
                                                    :a-rpcn-1-single-stage
                                                    :a-rpcn-unbalanced
                                                    :a-rpcn
                                                    :a-rpcn-1-unbalanced
                                                    :a-rpcn-1} %)
                                     attack-strategies)
                           ;; attack-strategies)
        ;;attack-strategies (if (<= (:independent-sigma io-attack-data) 0)
                            #_(remove #(contains? #{:a-rpin-unbalanced
                                                   :a-rpin
                                                   :a-rpin-1-unbalanced
                                                   :a-rpin-1} %)
                                     attack-strategies)
                           ;; attack-strategies)
        ]
    (zipmap
     attack-strategies (for [strategy attack-strategies]
                         (let [{:keys [attack-function cumulative-noise?  bias-input-prob? only-one-known?]} ;;independent-noise?
		                               (get (attack-strategy-map) strategy)
		                               start-time (get-current-thread-time!)
		                               attempts (doall (for [i (range attempt-count)]
		                                                 (attack-function
		                                                  io-attack-data (seeded-rng (+ base-attack-seed i)) ;window-no
		                                                  :optimization-max-evaluations optimization-max-evaluations
		                                                  :optimization-relative-threshold optimization-relative-threshold
                                                     ; :window-no window-no
		                                                  :cumulative-noise? cumulative-noise?
		                                                  ;;:independent-noise? independent-noise?
		                                                  :bias-input-prob? bias-input-prob?
		                                                  :only-one-known? only-one-known?)))
		                               stop-time (get-current-thread-time!)
		                               attempts (map #(assoc % :relative-error   (relative-error original-record
		                                                                                         (take (count original-record) (:optimum %)))
		                                                     :mean-attempt-cpu-nanoseconds
		                                                     (/ (- (:cpu-nano stop-time)
		                                                           (:cpu-nano start-time))
		                                                        attempt-count
		                                                      )
                                                       
		                                               )
		                                             attempts
		                                          )
		                               best-attempt (apply  max-key :score  attempts)
                               ]
                             best-attempt
                            ; (println attempts)
                             ; (println "best" best-attempt)
                           )
                        )
     ))
)



(defn calculate-AUC [lower-bound upper-bound growth-rate-k maximum-fn-value]
  "AUC is given by the difference of the integrals of the logistic function for upper and lower bound. 
   Since we add noise cumulatively AUC from lower bound to upper bound should be accounted cumulatively 
   to calculate total noise variance added within a logistic cycle"
; (def auc_cumulative (atom 0)) 
 ; (def auc_cumulative  (vec(repeat 1000 nil)))
  (def auc_cumulative (atom {:x {}}))
	(loop [i lower-bound]
		 (when (<= i upper-bound)
		   (let [auc (- (+ (* i maximum-fn-value) (* (/ (Math/log (m/abs (+ (Math/exp (* (* growth-rate-k i)-1)) 1) )) growth-rate-k) maximum-fn-value))
		                  (+ (* lower-bound maximum-fn-value) (* (/ (Math/log (m/abs (+ (Math/exp (* (* growth-rate-k lower-bound)-1)) 1) )) growth-rate-k) maximum-fn-value))
		               )
		         auc-list (swap! auc_cumulative assoc-in [:x i] {:id i :value auc})
		        ]
		    ; (println i, auc, auc-list)
		   )
		   (recur (+ i 1))
		 )
	)
 (def access-auc-list (-> @auc_cumulative :x))
 
  (loop [i lower-bound sum [] ]
    (if (<= i upper-bound)
      (recur (inc i) 
             (conj sum (get-in access-auc-list [i :value])) ;;Clojure arrays are immutable. Cannot simply append values. Should run through a loop
      ) 
    sum)
   )
; (println "list" @auc_cumulative)
)
#_ (- (+ (* upper-bound maximum-fn-value) (* (/ (Math/log (m/abs (+ (Math/exp (* (* growth-rate-k upper-bound)-1)) 1) )) growth-rate-k) maximum-fn-value))
     (+ (* lower-bound maximum-fn-value) (* (/ (Math/log (m/abs (+ (Math/exp (* (* growth-rate-k lower-bound)-1)) 1) )) growth-rate-k) maximum-fn-value))
  )
#_(let [ auc-vals (get-in access-auc-list [i :value])
            ]
        ; (println auc-vals)
        )

#_(defn calculate-lower-bound [fraction-initial-area growth-rate-k maximum-fn-value]
   "Calculate the lower bound of the logistic curve such that AUC from 0 to upper bound equals a fraction of AUC from lower bound to 0"
   ( / (Math/log (m/abs (/ (- 2 (Math/exp (/ (* growth-rate-k fraction-initial-area) maximum-fn-value))) (Math/exp (/ (* growth-rate-k fraction-initial-area) maximum-fn-value)))))
      growth-rate-k
    )
)

#_(defn calculate-upper-bound [fraction-initial-area growth-rate-k maximum-fn-value]
   (/ (Math/log (m/abs (- (* 2 (Math/exp (/ (* fraction-initial-area growth-rate-k ) maximum-fn-value))) 1)))
      growth-rate-k
    )
)

#_(defn calculate-bounds [growth-rate-k f maximum-fn-value cycle-size pre-upper-file pre-auc-file]
   "Calculating the upper area of the logistic curve given the initial cs values. Then Taking the fractions of that area and finding the lower (lb - 0) and upper (0 - ub)
    bounds to get the same AUC. Purpose is to find the lb and ub which provides the same AUC considering the mid point 0"
   (let [  mid-point 0
           ;;pre-upper (load-data pre-upper-file)
          ;; pre-upper-atom (atom pre-upper)
           initial-upper cycle-size
          growth-rate-k growth-rate-k
          ;;area-from-mid-point (calculate-AUC mid-point @pre-upper-atom growth-rate-k) ;; calculate the ares from mid point to upper bound
           initial-upper-area (calculate-AUC mid-point initial-upper growth-rate-k maximum-fn-value)
          fraction-initial-area (* initial-upper-area f) ;;a fraction of area from mid point, consider this to calculate both new lower and upper bounds
				   lower-bound (calculate-lower-bound fraction-initial-area growth-rate-k maximum-fn-value)  ;;area-from-mid-point = a fraction of area from a lower-bound to zero ----need to find lower bound using this
           upper-bound (calculate-upper-bound fraction-initial-area growth-rate-k maximum-fn-value)    
      
           up-area (calculate-AUC mid-point upper-bound growth-rate-k maximum-fn-value)
           lower-area (calculate-AUC lower-bound mid-point growth-rate-k maximum-fn-value)
           
					;;current-AUC (calculate-AUC lower-bound upper-bound growth-rate-k maximum-fn-value)   ;;calculate AUC of the logistic curve from the calculated lower bound to the upper bound 
           ;;epsilon 0.05 ;;throshold to move from the upper bound (to avoid the saturation complications)
          ;; pre-AUC (load-data pre-auc-file)
          
  ;;No need to check the convergence criteria since we are taking the fractions considering both lower and upper areas         
           ;;new-current-AUC(if (< (m/abs (- pre-AUC current-AUC )) epsilon)
                          #_  (do
                               (reset! pre-upper-atom (- @pre-upper-atom 5))
                                (calculate-AUC lower-bound @pre-upper-atom growth-rate-k maximum-fn-value)
                              )
                              ;; current-AUC
                            ;; )
				 ]
                        ;;(save-data pre-auc-file new-current-AUC)
                      ;;(save-data pre-upper-file @pre-upper-atom)
     ;; [@pre-upper-atom lower-bound]  
      [upper-bound lower-bound initial-upper-area up-area lower-area]
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;AUC can only be used as a measure of privacy when there's fixed cycle cize.
;;when the cycle sizes are randomly selected, it's difficult to use AUC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn evaluate-privacy-auc
  "Pricavy is measured using the area under the logistic curve (Sigma of added noise = F(x)* cumulative sigma)"
  [rng input-matrix masked-matrix projection-sigma cumulative-noise-sigma 
   translation noise-log upper-bound lower-bound growth-rate-k  maximum-fn-value output-file-privacy-auc
   {:keys [optimization-max-evaluations optimization-relative-threshold
            cycle-size evaluation-threads ;;previous-AUC-file-to-update  flat-record-length  f initial-upper upper-area lower-area
             ;;previous-upper-bound-file-to-update cycle-size-upper-bound pre-AUC ;; growth-rate-k maximum-fn-value
            ] :as privacy-evaluation-configuration
   }
  ]
	(with-open [writer (io/writer output-file-privacy-auc :append true)]
       (let [array-AUC (calculate-AUC lower-bound upper-bound  growth-rate-k maximum-fn-value)  ; this returns an array of cumulative auc values
             total-AUC-per-cycle (reduce + array-AUC) ; Summation of all the auc values
             auc-with-sigma (* total-AUC-per-cycle cumulative-noise-sigma)
             final-AUC;; (concat [["Growth-rate-k" "f-value" "upper-bound" "lower-bound" "privacy-measure"]]
						               (for [i (range 1)]
								                  [ growth-rate-k upper-bound lower-bound total-AUC-per-cycle auc-with-sigma] )
                    ;; )
             ]
					 ;  (println total-AUC-per-cycle)
				    (csv/write-csv writer final-AUC)         
			 )
    )                
  
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Evaluating privacy using attacks
(defn evaluate-privacy-attacks
  "Perform an attack-based evaluation of the privacy of the given masking using the given privacy-evaluation-configuration. Return a
  map that contains the attack-results of each evaluation performed, as well as relevant configuration information."
  [rng dataset input-matrix masked-matrix projection-sigma cumulative-noise-sigma translation noise-log growth-rate-k ;;independent-sigma
   {:keys [optimization-max-evaluations optimization-relative-threshold 
           attack-count attempt-count cycle-size ;;flat-record-length
           known-record-counts known-record-ranges known-record-range-position
          ; output-file-privacy-attacks 
           window-size record-count attack-strategies evaluation-threads]
    :as privacy-evaluation-configuration}
  ]
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Needed when attacks to evry cycle
  (def x (atom 0)) ;;waruni
  
  (def no-of-windows (int (ceil (/ record-count window-size ))) )
  (def full-record-count record-count)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; A separate evaluation for each prior knowledge configuration 
 {:configuration privacy-evaluation-configuration
 :evaluations
 ;(let [;configuration privacy-evaluation-configuration
    ; att-results 	
     (pmap-pool (or evaluation-threads 1)
		                 (fn [[known-record-count known-record-range rng]]
		                   (let [known-record-range-position (or known-record-range-position
		                                            :middle)
		                                        ]
		                      (debug
		                           (sync-println (str "Starting: privacy-evaluation"
		                           " -pf" (m/column-count masked-matrix)
		                           " -ps" projection-sigma
		                          ;; "-is" independent-sigma
                               " -cs" cumulative-noise-sigma
		                           " -tr" translation
		                          ;; " -cz" (* cycle-size 2)  ;Not useful when the cycle size is randomly selected
		                           " -krc" known-record-count
		                           " -krr" known-record-range
		                           " -ac"  attack-count
		                           " -krrp" known-record-range-position
                               " -gr" growth-rate-k
                               " -ws" window-size)))
		       ;; Record contains raw results for this
		                                   ;; prior-knowledge-configuration.
		       {:known-record-count known-record-count
		         :known-record-range known-record-range
		         :known-record-range-position known-record-range-position
		         :attack-results
          
		          (doall
		         (for [i (range (or attack-count 100))]
		           (let [;; We may need to try generate-io-attack-data multiple times, in case the first few selected records end
		                   ;; up being linearly dependent with all other records. 
                      window-no (if (<= i (* (int (/ attack-count no-of-windows)) (+ @x 1) )) ;;sending win-no to perform attacks for every window
				                                 @x  
				                              (swap! x inc)
				                             )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					             io-attack-data (try-times
					                                 100
							                                  (generate-io-attack-data! (seeded-rng (next-derived-seed! rng))
							                                                            input-matrix
							                                                            masked-matrix
							                                                            projection-sigma
							                                                            ;;independent-sigma
							                                                            cumulative-noise-sigma
							                                                            known-record-count
							                                                            known-record-range
							                                                            known-record-range-position 
                                                                          window-no  window-size
                                                                          full-record-count
							                                     ) 
                                       )
					                 original-record (m/get-row input-matrix
					                                            (get-in io-attack-data [:unknown :index]))
					                 known-indexes (map :index (:knowns io-attack-data))
					                 true-known-noise-differences (map #(reduce m/add (->> noise-log
					                                                                       (drop (inc %1))
					                                                                       (take (- %2 %1))))
					                                                   known-indexes (rest known-indexes))
					                 attack-result (perform-attack (seeded-rng (next-derived-seed! rng))
					                                               io-attack-data original-record ;window-no
					                                               (or attempt-count 100)
					                                               :optimization-max-evaluations optimization-max-evaluations
					                                               :optimization-relative-threshold optimization-relative-threshold
					                                               :attack-strategies attack-strategies)
		                ]
		             ;(println attack-result "attack-result")
		         ; (println "window", window-no)
		                  
                                {:strategies attack-result
                                 :window-no window-no
		                             :known-indexes (->> io-attack-data :knowns (map :index))
		                             :unknown-index (->> io-attack-data :unknown :index)
		                             :original-record (map double original-record)
		                             :true-known-noise-differences true-known-noise-differences
		                             :true-unknown-cumulative-noise
		                             (reduce m/add (take (:index (:unknown io-attack-data)) noise-log))
		                             }
		            )
             );; end for
            
		        ) ;end doall
                                                 
         }
		                                                )) 
		    ;; Attack configuration combinations.
                                 (for [known-record-count (or known-record-counts
		                                 (get-default-known-record-counts (m/column-count masked-matrix)))
		                             known-record-range (or known-record-ranges [0.1 0.5 1])]	           ;; Each configuration needs its own rng to prevent race
	                                 ;; conditions when multi-threading.
	                                 [known-record-count known-record-range (seeded-rng (next-derived-seed! rng))])
		                       )
     ; ] ;works
    ;(println att-results)
    ; );;works
}
)


(defn flatten-masking-experiment-recoveries
  "Take a sequence of outputs from masking-experiment, and return a
  flattened list of all record recoveries from attacks."
  [results]
  (for [result results
        evaluation (-> result :privacy :evaluations)
        attack (-> evaluation :attack-results)
        [strategy recovery] (-> attack :strategies)]
    (assoc recovery
           :strategy strategy
           :window-no (:window-no attack)
           :known-record-count (:known-record-count evaluation)
           :known-record-range (:known-record-range evaluation)
           :projection-features (:projection-features result)
           :projection-sigma (:projection-sigma result)
         ;;  :independent-noise-sigma (:independent-noise-sigma result)
           :cumulative-noise-sigma (:cumulative-noise-sigma result)
           :translation (:translation result)
           :known-indexes (:known-indexes attack)
           :unknown-index (:unknown-index attack))))



(defn unknown-record-displacement
  "Return the displacement of the unknown record from the known records
  in a flattened or attack result: the smallest difference between the
  unknown index and any known index."
  [result]
  (->> result
       :known-indexes
       (map #(Math/abs (- (:unknown-index result) %)))
       (reduce min)))

(defn unknown-record-relative-position
  [result]
  (cond
    (< (:unknown-index result) (apply min (:known-indexes result))) :before
    (> (:unknown-index result) (apply max (:known-indexes result))) :after
    :default :middle))

(defn prob-eps-privacy-breach
  "Return the probability of an epsilon-privacy breach - the proportion
  of recoveries with a relative error less than epsilon."
  [flat-results epsilon]
  (/ (count (filter #(< (:relative-error %) epsilon) flat-results))
     (count flat-results)
   )
 )

(defn map-attack-results
    [func results]
    (for [result results]
        (update-in result [:privacy-attacks :evaluations] ;;Changed waruni
                   (fn [evaluations]
                       (for [evaluation evaluations]
                           (update evaluation :attack-results
                                   (fn [attack-results]
                                       (map func attack-results))))))))

(defn add-combined-result
    [results metric strategies]
    (let [strategy-name (keyword (str (name metric) "_" (string/join ":" (map name strategies))))
          comparison-operator (case metric
                                  :relative-error min-key
                                  :score max-key
                                  :default (throw (Exception. "Unsupported combined result metric")))]
        (map-attack-results
            (fn [attack-result]
                (assoc-in attack-result [:strategies strategy-name]
                          (->> attack-result
                               :strategies
                               (filter #(contains? (set strategies) (first %)))
                               vals
                               (apply comparison-operator metric))))
            results)))

(defn get-cumulative-noise-sigma
  "Gets a cumulative-noise-sigma that results in an equivalent amount of
  total noise as the given independent-noise-sigma for a dataset with
  the given record-count."
  [independent-noise-sigma record-count]
  (/ (* record-count independent-noise-sigma)
     (->> (range 1 (inc record-count))
          (map sqrt)
          (reduce +))))

(defn get-attack-count [cycle-size]
  ( * (* cycle-size 2) 0.2)
  
 )

;;;;;;;;;;Waruni


(defn get-probabilities [masked-model-description]
  "access the path and class instances/probabilities of the tree from the clojure data structure"
  (case (:type masked-model-description)
    :branch (->> masked-model-description
                 :branches
                 vals
                 (map get-probabilities)
                 (apply concat))
    :leaf [{:class-instances (:class-instances masked-model-description)
           :instances (:instances masked-model-description)
           }] 
  )    
)

(defn calculate-avarage-information-loss-error [model-description probability-file & [k-value maximum-fn-value]]
  "Using leaf probabilities, path probabilities, leaf count and total number of records in each leaf node,
   calculate Average Expected Loss (AEL) for original and perturbed datasets. Write the calculated AEL to a csv file"
  (with-open [writer (io/writer probability-file :append true)]
		  (let [results (get-probabilities model-description)
            leaf-count (count results)
            instances (for [res results]
                         (:instances res)
                      )
            total-instances (reduce + instances) ;;total-classified-instances
            path-into-leaf (for [res results] ;;path-prob multiplied by incorrect leaf-prob
                             (* (- 1 (/ (:class-instances res)(:instances res)) ) (/ (:instances res) total-instances) )
                          )
            correct-instances (for [res results]
                                (:class-instances res)
                                )
            acc (/ (reduce + correct-instances) total-instances)
         ;  acc (/ (/ (reduce + correct-instances) total-instances)leaf-count) ;Average accuracy per leaf
            path-into-leaf-sum (reduce + path-into-leaf) ;;summation of the above
           ; AIL  (/ path-into-leaf-sum leaf-count) ;;Average AIL per leaf
           AIL  path-into-leaf-sum ;;AEL for the whole tree
            prob (if k-value
                        ;;(concat [[ "Growth-Rate-k" "AIL"]]
                       (for [i (range 1)]
			                            [k-value AIL (- 1 acc)]
			                          )
			                     ;;)
                       (concat [["Original Error"]]
				                           (for [i (range 1)]
							                            [AIL (- 1 acc)]
							                      )
                               )
                     )             
		        ]
		       (csv/write-csv writer prob)   
		  )
  )
)

(defn getting-original-model-description [dataset model-file loss-file]
  "Getting the model description as moa string and pass it to a clojure data structure.
   This is done because we need to access the elements of the data structure to calculate AEL and it's difficult using moa structure"
		(let [schema (get-schema dataset )
          model (hoeffding-tree  schema)
          record (-> dataset :records)
          record-count (count record)
         ; dataset-label "arem"
          ;file (str "workspace/" dataset-label "/original-model-description.edn")
        ]
		    (loop [i 0]
		      (when (< i  record-count)
		          (process-record model (nth record i))
		           (recur (+ i 1))
		        )
		     )
       (save-data model-file (parse-moa-tree-model-string (describe-model model)) )
     ;  (calculate-avarage-information-loss-error (load-data model-file) loss-file)
     )		  
)

;;Final loss
(defn getting-masked-model-description [masked-matrix schema classes masked-model-file  masked-probability-file growth-rate-k maximum-fn-value]
		(let [ model (hoeffding-tree  schema)
          masked-data (matrix->dataset schema masked-matrix classes)
          record (-> masked-data :records)
          record-count (count record)
        ]
    (loop [i 0]
      (when (< i  (- record-count 1))
          (process-record model (nth record i))
           (recur (+ i 1))
        )
     )
     (save-data masked-model-file (parse-moa-tree-model-string (describe-model model)) )
    ; (calculate-avarage-information-loss-error (load-data masked-model-file) masked-probability-file growth-rate-k maximum-fn-value)
     )		  
)
;;Loss for each record in the stream
(defn getting-loss-each-record [masked-matrix schema classes anytime-ael-file growth-rate-k maximum-fn-value]
		(let [ model (hoeffding-tree  schema)
          masked-data (matrix->dataset schema masked-matrix classes)
          record (-> masked-data :records)
          record-count (count record)
        ]
  
     (loop [i 0]
      (when (< i  (- record-count 1))
         (do 
           (process-record model (nth record i))
             (let [model (parse-moa-tree-model-string (describe-model model))
                   loss (calculate-avarage-information-loss-error model anytime-ael-file growth-rate-k maximum-fn-value)
                  ] 
               )
          )
         (recur (+ i 1))
        )
     )
     
     )		  
)

;;.............................
;; == Combined Evaluation ==
(defn masking-experiment-cycles
  [& {:keys [raw-dataset projection-features projection-sigma masked-model-file masked-probability-file anytime-ael-file
              cumulative-noise-sigma growth-rate-k maximum-fn-value  pre-upper-file;;independent-noise-sigma
             translation  cycle-size  classifier-fns pre-auc-file  output-file-privacy-auc;;cycle-size-upper-bound pre-AUC  bandwidth
             privacy-evaluation-configuration seed evaluations]}]
  
					 (let [rng (seeded-rng seed)
					        ;; We max/min normalise the dataset so that the effect of the same sigma (particularly for additive noise) is consistent  
                 ;;across different datasets. 
                   ;;This was previously done in Chen, K., Sun, G., & Liu, L. (2007, April). Toward attack-resilient geometric data perturbation. 
                   ;;In proceedings of the 2007 SIAM international conference on Data mining (pp. 78-89). Society for Industrial and Applied Mathematics.
                  ; f bandwidth
					        dataset (normalise-dataset raw-dataset)
					         noise-log-atom (atom [])
                 ; bound-values (calculate-bounds growth-rate-k f maximum-fn-value cycle-size pre-upper-file pre-auc-file)
                 ; upper-bound (nth bound-values 0)
                  ;lower-bound (nth bound-values 1)
                   ;initial-upper (nth bound-values 2)
                 ; upper-area (nth bound-values 3)
                 ; lower-area (nth bound-values 4)
                  upper-bound cycle-size
                   lower-bound (* cycle-size -1)
                  
               
					        dataset-mask-factory (make-random-projection-with-noise-and-translation-mask-factory-cycles
					                              projection-features projection-sigma
					                              cumulative-noise-sigma ;; independent-noise-sigma
					                             translation cycle-size growth-rate-k maximum-fn-value ;;upper-bound lower-bound  ;;waruni
					        :seed (next-derived-seed! rng)
					                              :noise-log-atom noise-log-atom)
					        masked-dataset (mask-dataset (make-dataset-mask dataset-mask-factory dataset) dataset)
					        input-matrix (dataset->matrix dataset)
					        masked-matrix (dataset->matrix masked-dataset)
					        evaluations (or evaluations [:privacy :accuracy ]) ;;changed waruni :privacy-attacks :privacy-auc
					   ;;;;;;; waruni
        
					       masked-schema (get-masked-schema (make-dataset-mask dataset-mask-factory dataset))
					       masked-classes  (dataset->classes masked-dataset)
 ;;Calculating AEL or Error from the tree is not valid for HAT as it removes/ replaces the branches.
; Should consider the error for whole stream
            ;;Calculating loss for each record in the data stream
               ;;  anytime-loss (getting-loss-each-record masked-matrix masked-schema masked-classes anytime-ael-file growth-rate-k maximum-fn-value)
                 
                 ;;To calculate the final loss
                 model-description (getting-masked-model-description masked-matrix masked-schema masked-classes masked-model-file masked-probability-file growth-rate-k maximum-fn-value)
              
                 
;;privacy-auc is difficult to measure for the random cycle sizes. Ignore it at this stage
                 privacy-auc (evaluate-privacy-auc rng input-matrix masked-matrix projection-sigma ;;independent-noise-sigma  f
					                               cumulative-noise-sigma translation @noise-log-atom upper-bound lower-bound growth-rate-k  maximum-fn-value 
                                           output-file-privacy-auc privacy-evaluation-configuration ) ;initial-upper upper-area lower-area
               
					  ;;;;;; 
					          output (zipmap
					               evaluations
					                (for [evaluation evaluations]
					                  (case evaluation
                               :privacy(evaluate-privacy-attacks rng dataset input-matrix masked-matrix projection-sigma
                                                cumulative-noise-sigma translation @noise-log-atom growth-rate-k ;;independent-sigma 
                                             privacy-evaluation-configuration )
					                      :accuracy (map-vals #(test-classification-accuracy % masked-dataset) classifier-fns )    ;;write the accuray after masking to the file in the test-classification-accuracy
                                 
                        
                            #_ :privacy-auc #_(evaluate-privacy-auc rng input-matrix masked-matrix
					                                                     projection-sigma ;;independent-noise-sigma
					                                                     cumulative-noise-sigma translation
					                                                     @noise-log-atom upper-bound lower-bound f growth-rate-k  maximum-fn-value 
                                                               initial-upper upper-area lower-area
					                                                 privacy-evaluation-configuration
                                                    )
                            )
                         )
                    )
                
					          ]
      ;  (println "Masked dataset to training" masked-matrix)
         
        (assoc output
           :projection-features projection-features
           :projection-sigma projection-sigma
           ;;:independent-noise-sigma independent-noise-sigma
          :cumulative-noise-sigma cumulative-noise-sigma
           :translation translation
           :seed seed
           :growth-rate growth-rate-k)
         
         
         
       ;(recur (Double/parseDouble(format "%.2f" (+ f 0.02))))
		 )
	
)


