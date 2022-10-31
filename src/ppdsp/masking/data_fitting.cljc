(ns ppdsp.masking.data_fitting
  (:require  [clojure.core.matrix :as m]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.core.matrix.linear :as ml]
            [clojure.core.matrix.protocols :as mp]
            [clojure.math.numeric-tower ::as Math]
            ;[vizard [core :refer :all] [plot :as plot]]
            ;[cljplot.build :as b]
            ;[cljplot.core :as cc] ;
            [com.hypirion.clj-xchart :as clj]
            [fastmath.core :as fm]
            [fastmath.kernel :as k]
            [fastmath.random :as r]
            [fastmath.regression :as reg]
            [clojure2d.color :as c]
          ;  [smile.regression.GaussianProcessRegression :as grs]
            ;;
    )
  (:use [incanter.charts :only [histogram]]
        [incanter.core :only [to-matrix sel plus minus mult mmult div exp sq ncol ]]
        [incanter.stats :only [linear-model]]
        [incanter.optimize]
        [incanter.datasets]
        [incanter.io]
   )
)
;;(System/setProperty "java.awt.headless" "true")
(defn function-linear-privacy [parameters x]
  "Privacy (BP) is measured over seven selected growth rate (k) values.
  A polynomial (3rd degree)/cubic function is going to be fitted to the results of the experiments to find out a generic formula to represent the
  behaviuor of BP (y) values with k (x). Most suitable way is to fit data to a 4th degree/quartic functoin, but it's more complicated and finding roots are also more complicated.
   And all four roots can be complex numbers which stops further calculatoins"
  (let [[a b c d ] parameters]
       ;(plus (mult a (mult x (sq x))) (plus (mult b (sq x)) (plus (mult c x) d) ))
       (+ (* a x x x) (* b x x) (* x c) d)
    )
)
(defn function-linear-accuracy [parameters x]
  "Accuracy (AEL)  is measured over seven selected growth rate (k) values.
  A polynomial (4th degree)/quartic function is going to be fitted to the results of the experiments to find out a generic formula to represent the
  behaviuor of AEL values with k (x) "
  (let [[ a b c d e] parameters]
    ;(plus (mult a (sq (mult x x))) (plus (mult b (mult x (sq x))) (plus (mult c (sq x)) (plus (mult d x) e) )))
    (+ (* a x x x x) (* b x x x) (* c x x) (* d x) e)
   
  )
)
#_(defn function-nonlinear [parameters x]
  "Accuracy (AEL) and Privacy (BP) is measured over seven selected growth rate (k) values.
  A Gaussian kernel/function is going to be fitted to the results of the experiments to find out a generic formula to represent the
  behaviuor of AEL & BP (y) values with k (x) "
  (let [[a b] parameters]
     ;(exp (mult -1 (div (sq (minus x a)) (mult (* b b) 2)))) ;; Gives an error. 
     ;(exp (div (sq (minus a x)) (* (mult b b) -2))) ;How ever there's no use of fitting the data into a Gaussian kernel as it's been used to make a dataset linearly separable. Not for data fitting
  )
)
;;Calculating roots of a cubic function using Newton's method
(defn polynomial [a b c d]
  "Returns the polynomial function according to the given coefficients (f(x))"
    (fn[x] (+ (* a x x x) (* b x x) (* x c) d))
)
    
(defn polynomial-first-derivative [a b c d ]
 "Returns the first derivative of the polynomial function according to the given coefficients (f'(x))"
   (fn[x] (+ (* a 3 x x) (* b 2 x) c))
    ;; 3ax^2 + 2bx + c 
)

(defn => [f f' x]
  "Update function which iteratively updates the solution using a formula (xn+1 = xn - (f(x)/f'(x))) based on the initial/current guess."
    (f x) (f' x)
    (/ (f x)(f' x))
    (- x (/ (f x) (f' x)))
)

(defn newtons-method [f f' n x0]
  "Implementation of Newton's method to solve f(x) = 0, which takes function, first derivative, number of iterations and initial guess as it's arguments "
    (def nextx (=> f f' x0))
    (loop [i n]
        (when (> i 1)
            (def nextx (=> f f' nextx))
            (recur (- i 1))))
    [nextx (f nextx)]
)

(defn calculate-k-for-privacy-threshold [user-threshold pri-coefs]
  "Calculate the respective growth rate value (k) for the privacy threshold given by the user
   using the estimated data fitting function and parameters Breach probability. 
   To find the roots, an approximation root finding method 'Newton's Method' proposed by Issac Newton and Joseph Raphson was used.
   If the euation is quartic ferrari's method can be used to find the roots, but complicated and complex"
  (let [a (nth pri-coefs 0)
        b (nth pri-coefs 1)
        c (nth pri-coefs 2)
        d (nth pri-coefs 3)
       ; initial-k-value (atom 0.0)
        bp-according-to-user (- 1.0 user-threshold) ;we calculated bp not privacy. So convert user's privacy threshold in to a bp value
        dnew (- d bp-according-to-user) ; organizr the equation as f(x) = 0
        function (polynomial a b c dnew)
        first-derivative (polynomial-first-derivative a b c dnew)
        num-iterations 20
        initial-guess 0.0
        solutions (newtons-method function first-derivative num-iterations initial-guess)
        selected-k-value (nth solutions 0)
       #_ (for [solution solutions]
                           (if (>= solution 0.0015)
	                            (reset! initial-k-value solution )
	                            (reset! initial-k-value (nth solutions 0) )
                           )
                          )
        ]
    ;(println "solutions" solutions)
    ;(println "bp" bp-according-to-user)
   ; (println "dnew" dnew)
    
    selected-k-value
 ; (println (nth solutions 0))
    )
)

(defn calculate-AEL-for-k [k-value-for-privacy-threshold acc-coefs]
  "Calculate the respective accuracy (AEL) for the k value calculated according to the privacy threshold
   using the estimated data fitting function and parameters for AEL"
  (let [k k-value-for-privacy-threshold
        a (nth acc-coefs 0)
        b (nth acc-coefs 1)
        c (nth acc-coefs 2)
        d (nth acc-coefs 3)
        e (nth acc-coefs 4)
        
     AEL (+ (* a k k k k) (* b k k k) (* c k k) (* d k) e)
     ; AEL (+ (* a k k k) (* b k k) (* k c) d)
        ]
      AEL
    )
)

;;calculating the auc threshold using the user given privacy threshold
(defn calculate-auc-threshold [y-auc user-threshold ] ;cycle-size maximum-fn-value
  "User threshold is considered as the min-max normalized value of the actual AUC in this context.
   Therefore user threshold is used in reverse min-max normalization to find the actual AUC"
  (let [max-y-auc (apply max y-auc)
        min-y-auc (apply min y-auc)
        threshold-to-auc (+ (* user-threshold (- max-y-auc min-y-auc)) min-y-auc)
       ]
    threshold-to-auc
   )
)
;;;;;;;;Implementing Gaussian Kernel
(defn gaussian-kernel [x-values smooth-factor]
  "Kernel regression is a non parametric regression technique which doesn't use any underlying assumption.
   It uses identical weighted function for each data point based on the distance.
   We use Gaussian kernel as the data fitting function and 1 / (sigma * sqrt(2 * pi))) will cancel out when calculating the weighted average."
  (exp (/ (* -0.5 x-values x-values) (* smooth-factor smooth-factor) )); 
)

(defn kernel-regression [x y variance-smooth threshold]
  "Here we find the respective k value for the use given privacy threshold.For privacy  x-->bp values and y-->k values.
   And also respective AEL for the k value returned for the user given privacy threshold. For accuracy x-->k values and y-->AEL values"
  (let [  x-values x
          y-values y
          smooth-factor variance-smooth
          value-to-be-tested threshold
          gauss-kernels (for [x-values x-values]
                          (gaussian-kernel (- x-values value-to-be-tested) smooth-factor) ;calculating the goussian kernel for each dats point
                         )
          sum-kernels (reduce + gauss-kernels) ;summation of kernels
          weights (for [kernels gauss-kernels] ;;weight od each data point
                    (/ kernels sum-kernels )
                   )
          return-value (reduce + (map * y-values weights))  ;dot product of y and weights ;(mmult y-values weights) -->this gives the answer as a vector scalar
		         ; xx (println "kernels" gauss-kernels) 
		         ; xxx (println "sum" sum-kernels)
		         ; yy (println "weights" weights)
		         ; yyy   (println "return-value" return-value)
		          
         ]
    ;(println gauss-kernels sum-kernels)
    return-value
     )
)
;;;;;;;;
(defn normalize-results [y-auc] 
   "Normalizing the cumulative AUC values so it can be represented as a percentage of noise variance added 
    in the experimented noise addition rate values "
   (let [max-y-auc (apply max y-auc)
         min-y-auc (apply min y-auc)
         y-auc-list (apply list y-auc)
       ;  min-max []
        ]
       (loop [i 0 min-max [] ]
          (if (< i (count y-auc-list)) ;;(ncol y-auc)
             (recur (inc i) 
                     (conj min-max (double (/(- (nth y-auc-list i) min-y-auc)(- max-y-auc min-y-auc))))
              )
           min-max )
       )
       
    )
)
(defn k-AEL-final-from-selected-kernel [pri-file acc-file auc-file privacy-threshold-by-user final-AEL-file]
 "Selected kernel methods for regression after comparing five kernel methods, are used to determine the final results 
   using indirect regression method.
   For AEL-BP selected kernel -->Wave  For AEL-AUC selected kernel -->"
 (with-open [writer (io/writer final-AEL-file :append true)]
   (let [   acc-data (to-matrix (read-dataset acc-file))
					 pri-data (to-matrix (read-dataset pri-file))
			     auc-data (to-matrix (read-dataset auc-file))
					 x (sel acc-data :cols 0) ;k-values
					 y-acc (sel acc-data :cols 1) ;ael values in form of vectors
					 y-pri (sel pri-data :cols 1) ;bp values
			     y-auc (sel auc-data :cols 4) ;auc values
			     y-auc-new (normalize-results y-auc)
         ;;;;;;;;;;;;;AEL-BP Regression
          pri-bp-regression (reg/gaussian-process {:kernel (k/kernel :wave 0.01) 
			                                     :lambda 0.0001} y-pri y-acc ) 
			    ael-bp-regression (m/abs (reg/predict pri-bp-regression (- 1 privacy-threshold-by-user)) ) ;find ael from pri-bp
       
			   k-regression-bp (reg/gaussian-process {:kernel (k/kernel :wave 0.01) ;find respective k for above ael
			                                    :lambda 0.0001} y-acc x )  
			   ael-k-regression-bp (m/abs (reg/predict k-regression-bp ael-bp-regression) )
      ;;;;;;;;;;AEL-AUC Regression
         pri-auc-regression (reg/gaussian-process {:kernel (k/kernel :wave) 
			                                        :lambda 0.0001} y-auc-new y-acc ) 
			   ael-auc-regression (m/abs (reg/predict pri-auc-regression privacy-threshold-by-user) );find ael from pri-auc
			   k-regression-auc (reg/gaussian-process {:kernel (k/kernel :wave )
			                                      :lambda 0.0001} y-acc x)  
			   ael-k-regression-auc (m/abs (reg/predict k-regression-auc ael-auc-regression) );find respective k for above ael
      ;;;;
         values (for [i (range 1)]
                  [privacy-threshold-by-user ael-bp-regression ael-k-regression-bp ael-auc-regression ael-k-regression-auc]
                )
       
         ]
       (csv/write-csv writer values)
   )
 )
)
;;;Gaussian Kernel using FASTMATH -Direct
(defn kernel-regression-using-fastmath [pri-file acc-file user-threshold-testing datafitting-results-file auc-file ]
  "Results of the five different kernels were compared to select the most suitable kernels for regression with 
    both AEL-BP and AEL-AUC scenario. Regression is conducted and results were written to the results-data-fitting.csv.
     Visualization of the kernel regression is conducted separately in curve fitting module."
  (with-open [writer (io/writer datafitting-results-file :append true)]
    (doall (for [user-threshold user-threshold-testing]
			    (let [ ;;;Reading data and preparing
			          acc-data (to-matrix (read-dataset acc-file))
						    pri-data (to-matrix (read-dataset pri-file))
			          auc-data (to-matrix (read-dataset auc-file))
					      x (sel acc-data :cols 0) ;k-values
					      y-acc (sel acc-data :cols 1) ;ael values in form of vectors
					      y-pri (sel pri-data :cols 1) ;bp values
			          y-auc (sel auc-data :cols 4) ;auc values
			          y-auc-new (normalize-results y-auc)
			      
			    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Direct -Gaussian
			         acc-bp-regression1 (reg/gaussian-process {:kernel (k/kernel :gaussian 0.01 )
			                                      :lambda 0.001} y-pri y-acc)
			         bp-direct-regression1 (m/abs (reg/predict acc-bp-regression1 (- 1 user-threshold)))
              ;;;;;;AUC
               acc-auc-regression1 (reg/gaussian-process {:kernel (k/kernel :gaussian ) ; for auc-ael plots default scaling parameter ,1
			                                      :lambda 0.001} y-auc-new y-acc)
			         auc-direct-regression1 (m/abs (reg/predict acc-auc-regression1 user-threshold))
              
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Direct - rational-quadratic
                acc-bp-regression2 (reg/gaussian-process {:kernel (k/kernel :rational-quadratic 0.01)
			                                      :lambda 0.001} y-pri y-acc)
			           bp-direct-regression2 (m/abs(reg/predict acc-bp-regression2 (- 1 user-threshold)))
              ;;;;;;AUC
               acc-auc-regression2 (reg/gaussian-process {:kernel (k/kernel :rational-quadratic)
			                                      :lambda 0.001} y-auc-new y-acc)
			         auc-direct-regression2 (m/abs (reg/predict acc-auc-regression2 user-threshold))
         ;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Direct - laplacian
                acc-bp-regression3 (reg/gaussian-process {:kernel (k/kernel :laplacian 0.01)
			                                      :lambda 0.001} y-pri y-acc)
			           bp-direct-regression3 (m/abs(reg/predict acc-bp-regression3 (- 1 user-threshold)))              
              ;;;;;;AUC
              acc-auc-regression3 (reg/gaussian-process {:kernel (k/kernel :laplacian)
			                                      :lambda 0.001} y-auc-new y-acc)
			         auc-direct-regression3 (m/abs (reg/predict acc-auc-regression3 user-threshold))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Direct - wave
                acc-bp-regression4 (reg/gaussian-process {:kernel (k/kernel :wave 0.01)
			                                      :lambda 0.001} y-pri y-acc)
			           bp-direct-regression4 (m/abs(reg/predict acc-bp-regression4 (- 1 user-threshold)))
              ;;;;;;AUC
              acc-auc-regression4 (reg/gaussian-process {:kernel (k/kernel :wave)
			                                      :lambda 0.001} y-auc-new y-acc)
			         auc-direct-regression4 (m/abs (reg/predict acc-auc-regression4 user-threshold))
              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Direct - mattern-52
               acc-bp-regression5 (reg/gaussian-process {:kernel (k/kernel :mattern-52 0.01)
			                                      :lambda 0.001} y-pri y-acc)
			           bp-direct-regression5 (m/abs (reg/predict acc-bp-regression5 (- 1 user-threshold)))
              ;;;;;;AUC
              acc-auc-regression5 (reg/gaussian-process {:kernel (k/kernel :mattern-52 )
			                                      :lambda 0.001} y-auc-new y-acc)
			         auc-direct-regression5 (m/abs (reg/predict acc-auc-regression5 user-threshold))
              
              ;;;;;;;;;;;;;;;
                 values-write (for [i (range 1)]
                                [user-threshold bp-direct-regression1 auc-direct-regression1 bp-direct-regression2 auc-direct-regression2 bp-direct-regression3 auc-direct-regression3 
                                  bp-direct-regression4 auc-direct-regression4 bp-direct-regression5 auc-direct-regression5] ;k-pri-bp-regression ael-acc-regression
                                )
               
               ;;;;;;;
			         ]
			   (csv/write-csv writer values-write)
      
        ; (println y-auc)
        ; (println  y-auc-new " y-auc-new")
        ; (println acc-bp-regression1  "regression")
			     )
       )
    )
   )
 )
#_(defn kernel-regression-using-fastmath [pri-file acc-file user-threshold-testing datafitting-results-file auc-file ]
   "Results of the five different kernels were compared to select the most suitable kernels for regression with 
    both AEL-BP and AEL-AUC scenario. Regression is conducted and results were written to the results-data-fitting.csv.
     Visualization of the kernel regression is conducted separately in curve fitting module."
   (with-open [writer (io/writer datafitting-results-file :append true)]
     (doall (for [user-threshold user-threshold-testing]
			     (let [ ;;;Reading data and preparing
			            acc-data (to-matrix (read-dataset acc-file))
						    pri-data (to-matrix (read-dataset pri-file))
			          auc-data (to-matrix (read-dataset auc-file))
					      x (sel acc-data :cols 0) ;k-values
					        y-acc (sel acc-data :cols 1) ;ael values in form of vectors
					        y-pri (sel pri-data :cols 1) ;bp values
			            y-auc (sel auc-data :cols 4) ;auc values
			            y-auc-new (normalize-results y-auc)
			      
			     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Direct -Gaussian
			       ;;;;;;;;;;;;;AEL-BP Regression
          pri-bp-regression1 (reg/gaussian-process {:kernel (k/kernel :gaussian 0.01) ;name of the kernel and scaling parameter
			                                      :lambda 0.001} y-pri x ) 
			    k-pri-bp-regression1 (reg/predict pri-bp-regression1 (- 1 user-threshold)) ;find k for user threshold
			    ael-regression-bp1 (reg/gaussian-process {:kernel (k/kernel :gaussian 0.01)
			                                      :lambda 0.001} x y-acc )  ;;lambda ia the noise factor (smoothness)
			    ael-k-regression-bp1 (reg/predict ael-regression-bp1 k-pri-bp-regression1) ;find AEL for k
       ;;;;;;;;;;AEL-AUC Regression
          pri-auc-regression1 (reg/gaussian-process {:kernel (k/kernel :gaussian 0.01) 
			                                        :lambda 0.001} y-auc-new x ) 
			    k-pri-auc-regression1 (reg/predict pri-auc-regression1 user-threshold) ;find k for user threshold
			    ael-regression-auc1 (reg/gaussian-process {:kernel (k/kernel :gaussian 0.01)
			                                      :lambda 0.001} x y-acc )  
			    ael-k-regression-auc1 (reg/predict ael-regression-auc1 k-pri-auc-regression1) ;find AEL for k    
        
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Direct - rational-quadratic
             pri-bp-regression2 (reg/gaussian-process {:kernel (k/kernel :rational-quadratic 0.01) ;name of the kernel and scaling parameter
			                                      :lambda 0.001} y-pri x ) 
			    k-pri-bp-regression2 (reg/predict pri-bp-regression2 (- 1 user-threshold)) ;find k for user threshold
			    ael-regression-bp2 (reg/gaussian-process {:kernel (k/kernel :rational-quadratic 0.01)
			                                      :lambda 0.001} x y-acc )  ;;lambda ia the noise factor (smoothness)
			    ael-k-regression-bp2 (reg/predict ael-regression-bp2 k-pri-bp-regression2) ;find AEL for k
       ;;;;;;;;;;AEL-AUC Regression
          pri-auc-regression2 (reg/gaussian-process {:kernel (k/kernel :rational-quadratic 0.01) 
			                                        :lambda 0.001} y-auc-new x ) 
			    k-pri-auc-regression2 (reg/predict pri-auc-regression2 user-threshold) ;find k for user threshold
			    ael-regression-auc2 (reg/gaussian-process {:kernel (k/kernel :rational-quadratic 0.01)
			                                      :lambda 0.001} x y-acc )  
			    ael-k-regression-auc2 (reg/predict ael-regression-auc2 k-pri-auc-regression2) ;find AEL for k  
        
         ;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Direct - laplacian
                 pri-bp-regression3 (reg/gaussian-process {:kernel (k/kernel :laplacian 0.01) ;name of the kernel and scaling parameter
			                                      :lambda 0.001} y-pri x ) 
			    k-pri-bp-regression3 (reg/predict pri-bp-regression3 (- 1 user-threshold)) ;find k for user threshold
			    ael-regression-bp3 (reg/gaussian-process {:kernel (k/kernel :laplacian 0.01)
			                                      :lambda 0.001} x y-acc )  ;;lambda ia the noise factor (smoothness)
			    ael-k-regression-bp3 (reg/predict ael-regression-bp3 k-pri-bp-regression3) ;find AEL for k
       ;;;;;;;;;;AEL-AUC Regression
          pri-auc-regression3 (reg/gaussian-process {:kernel (k/kernel :laplacian 0.01) 
			                                        :lambda 0.001} y-auc-new x ) 
			    k-pri-auc-regression3 (reg/predict pri-auc-regression3 user-threshold) ;find k for user threshold
			    ael-regression-auc3 (reg/gaussian-process {:kernel (k/kernel :laplacian 0.01)
			                                      :lambda 0.001} x y-acc )  
			    ael-k-regression-auc3 (reg/predict ael-regression-auc3 k-pri-auc-regression3) ;find AEL for k
        
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Direct - wave
                   pri-bp-regression4 (reg/gaussian-process {:kernel (k/kernel :wave 0.01) ;name of the kernel and scaling parameter
			                                      :lambda 0.001} y-pri x ) 
			    k-pri-bp-regression4 (reg/predict pri-bp-regression4 (- 1 user-threshold)) ;find k for user threshold
			    ael-regression-bp4 (reg/gaussian-process {:kernel (k/kernel :wave 0.01)
			                                      :lambda 0.001} x y-acc )  ;;lambda ia the noise factor (smoothness)
			    ael-k-regression-bp4 (reg/predict ael-regression-bp4 k-pri-bp-regression4) ;find AEL for k
       ;;;;;;;;;;AEL-AUC Regression
          pri-auc-regression4 (reg/gaussian-process {:kernel (k/kernel :wave 0.01) 
			                                        :lambda 0.001} y-auc-new x ) 
			    k-pri-auc-regression4 (reg/predict pri-auc-regression4 user-threshold) ;find k for user threshold
			    ael-regression-auc4 (reg/gaussian-process {:kernel (k/kernel :wave 0.01)
			                                      :lambda 0.001} x y-acc )  
			    ael-k-regression-auc4 (reg/predict ael-regression-auc4 k-pri-auc-regression4) ;find AEL for k
        
               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Direct - mattern-52
                pri-bp-regression5 (reg/gaussian-process {:kernel (k/kernel :mattern-52 0.01) ;name of the kernel and scaling parameter
			                                      :lambda 0.001} y-pri x ) 
			    k-pri-bp-regression5 (reg/predict pri-bp-regression5 (- 1 user-threshold)) ;find k for user threshold
			    ael-regression-bp5 (reg/gaussian-process {:kernel (k/kernel :mattern-52 0.01)
			                                      :lambda 0.001} x y-acc )  ;;lambda ia the noise factor (smoothness)
			    ael-k-regression-bp5 (reg/predict ael-regression-bp5 k-pri-bp-regression5) ;find AEL for k
       ;;;;;;;;;;AEL-AUC Regression
          pri-auc-regression5 (reg/gaussian-process {:kernel (k/kernel :mattern-52 0.01) 
			                                        :lambda 0.001} y-auc-new x ) 
			    k-pri-auc-regression5 (reg/predict pri-auc-regression5 user-threshold) ;find k for user threshold
			    ael-regression-auc5 (reg/gaussian-process {:kernel (k/kernel :mattern-52 0.01)
			                                      :lambda 0.001} x y-acc )  
			    ael-k-regression-auc5 (reg/predict ael-regression-auc5 k-pri-auc-regression5) ;find AEL for k
               
               ;;;;;;;;;;;;;;;
                   values-write (for [i (range 1)]
                                 [user-threshold ael-k-regression-bp1 ael-k-regression-auc1 ael-k-regression-bp2 ael-k-regression-auc2 ael-k-regression-bp3 ael-k-regression-auc3 
                                   ael-k-regression-bp4 ael-k-regression-auc4 ael-k-regression-bp5 ael-k-regression-auc5] ;k-pri-bp-regression ael-acc-regression
                                  )
               
                ;;;;;;;
			           ]
			    (csv/write-csv writer values-write)
      
         ; (println y-auc)
          ; (println  y-auc-new " y-auc-new")
          ; (println acc-bp-regression1  "regression")
			       )
        )
     )
    )
  )
;;;
#_(defn retrieve-accuracy-using-fitted-function [pri-file acc-file privacy-threshold-by-user final-results-file auc-file variance-smooth-bp variance-smooth-auc ];cycle-size maximum-fn-value
    "Selecting the necessary columns (AEL, BP and K) from the csv files and calculate the linear function which explains the data.
   This estimates parameters of the function by performing linear regression"
  (with-open [writer (io/writer final-results-file :append true)]
  (doall (for [user-threshold privacy-threshold-by-user]  ;;used doall to avoid error caused by lazy sequence 
		 (let [ ;;;Reading data and preparing
            acc-data (to-matrix (read-dataset acc-file))
			      pri-data (to-matrix (read-dataset pri-file))
            auc-data (to-matrix (read-dataset auc-file))
		        x (sel acc-data :cols 0) ;k-values
		        y-acc (sel acc-data :cols 1) ;ael values
		        y-pri (sel pri-data :cols 1) ;bp values
            y-auc (sel auc-data :cols 4) ;auc values
           ; variance-smooth 0.001 ;;smaller ->overfitting, larger ->underfitting
            ;;;;;;;;;;;
          ;  k-from-pri-auc-function (calculate-k-using-auc y-auc user-threshold ) ;cycle-size maximum-fn-value
            auc-user-threshold (calculate-auc-threshold y-auc user-threshold )
           
          ;  k-from-pri-bp-regression (kernel-regression y-pri x variance-smooth (- 1 user-threshold));calculate k for bp threshold
          ;  ael-from-acc-regression (kernel-regression x y-acc variance-smooth k-from-pri-bp-regression) ;calculate ael using above k
            direct-bp-ael-regression (kernel-regression y-pri y-acc variance-smooth-bp (- 1 user-threshold)) ;apply gaussian kernel directly to bp and eal
            direct-auc-ael-regression (kernel-regression y-auc y-acc variance-smooth-auc  auc-user-threshold) ;aaply gaussian kernel directly to auc and ael
           
            values-to-write (for [i (range 1)]
                              [user-threshold variance-smooth-bp direct-bp-ael-regression variance-smooth-auc direct-auc-ael-regression] ;k-from-pri-bp-regression ael-from-acc-regression 
                            )
           
            ;;;;;;;;; start Non-linear regression
												 ; start-linear-privacy [0.0001 0.0001 0.0001 0.0001]  
									       ; start-linear-accuracy [0.0001 0.0001 0.0001 0.0001 0.0001]
												 ; fitted-func-acc (non-linear-model function-linear-accuracy y-acc x start-linear-accuracy)
									       ; fitted-func-pri (non-linear-model function-linear-privacy y-pri x start-linear-privacy)    
									       ; acc-coefs (:coefs fitted-func-acc) ;;(60930.322548824675 -11928.230052875402 723.9907369582888 -14.737554304762323 0.17754057322488415)
									      ;  acc-rss (:rss fitted-func-acc) ;;0.002148966726082427
									      ;  pri-coefs (:coefs fitted-func-pri) ;;(-14517.282511879532 2325.2237768904993 -96.41871922894771 0.8557079188255589 0.02293506172042276)
									       ; pri-rss (:rss fitted-func-pri) ;;4.463849695980421E-5
									        
									       ; k-from-bp-threshold (calculate-k-for-privacy-threshold user-threshold pri-coefs)
									      ;  final-accuracy-using-bp    (calculate-AEL-for-k k-from-bp-threshold acc-coefs)
							            ;final-accuracy-using-auc    (calculate-AEL-for-k k-from-auc-function acc-coefs)
							          
							           ; values-to-write (concat [["acc-rss" "pri-rss" "user's privacy threshold" "calculated-k" "AEL for threshold"  ]]
							                             ; (for [i (range 1)]
															                ;  [acc-rss pri-rss user-threshold k-from-bp-threshold final-accuracy-using-bp])
							                           ; )
            ;;;;;;;;;;;;;; end Non-linear regression      
		       
		       ]
      (csv/write-csv writer values-to-write)

		 )
   )
     )
  )
)
