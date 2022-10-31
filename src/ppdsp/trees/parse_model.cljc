(ns ppdsp.trees.parse-model
  (:require [clojure.string :as string]))

(def indent-step 2)

(defn- indent-string [indent]
  (apply str (repeat indent " ")))

(defn- subtree [tree indent]
  (take-while #(string/starts-with? % (indent-string indent)) tree))

(defn- if-nominal-regex [indent]
  (re-pattern (str (indent-string indent)
                   "if \\[att [0-9]+:(.+)\\] = \\{val [0-9]+:(.+)\\}:.*")))

(defn- if-numeric-regex [indent]
  (re-pattern (str (indent-string indent)
                   "if \\[att [0-9]+:(.+)\\] ([<>=]+) (-?[0-9\\.]+):.*")))  ;;should change for negative numbers  -? added waruni

(defn- leaf-regex [indent]
  (re-pattern (str (indent-string indent)
                   "Leaf \\[class:(.+)\\] = <class [0-9]+:(.+)> weights: \\{([0-9\\.,\\|]+)\\}.*")))

(defn- parse-leaf-node [line indent]
  (let [[_ label class weights-string] (re-matches (leaf-regex indent) line)
        weights (->> (string/split weights-string #"\|")
                     (map #(string/replace % #"," ""))
                     (map read-string))]
    {:type :leaf
     :class-label label
     :class class
     :weights weights
     :instances (reduce + weights)
     :class-instances (apply max weights)}
   )
)

(declare lines->node)

(defn- parse-split-node [lines indent]
  (let [ifs (->> lines
                 (map-indexed
	                  (fn [idx line]
	                    (if-let [[_ feature val] (re-matches (if-nominal-regex indent) line)]
	                      {:idx idx :feature feature :val val}
	                      (if-let [[_ feature comparator val] (re-matches (if-numeric-regex indent) line)]
	                        {:idx idx :feature feature :val (str comparator " " val)}
	                        nil
                         )
                      )
                    )
                  )
                 (filter (comp not empty?))
             )
        branches (map (fn [if-start if-end]
                        (let [start (+ 1 if-start)
                              length (- if-end start)]
                          (->> lines
                               (drop start)
                               (take length))))
                      (map :idx ifs) (conj (vec (map :idx (rest ifs)))
                                           (count lines))
                  )
        ]
    (when (> (count (set (map :att ifs))) 1)
      (println "WARNING: Multi-attribute split at a single node.")
    )
    {:type :branch
     :feature (:feature (first ifs))
     :branches (into {} (map #(vector (:val %1)
                                      (lines->node %2 (+ indent indent-step)))
                             ifs branches)
               )
     }
   )
)

(defn- lines->node [lines indent]
  (if (= 1 (count lines))
    (parse-leaf-node (first lines) indent)
    (parse-split-node lines indent)
   )
)

(defn parse-moa-tree-model-string
  "Given a model string description from a MOA tree (e.g. Hoeffding
  tree), parse it to build a representation with Clojure data
  structures."
  [model-string]
  (-> model-string
    (string/replace "\r" "")
    (string/split #"\n")
    (lines->node indent-step)))
