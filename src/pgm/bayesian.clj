(ns pgm.bayesian
	(use pgm.factor clojure.set clojure.math.numeric-tower)
	(require [loom.graph :as gr]
			 [loom.alg :as gr-al]
			 [clojure.math.combinatorics :as comb]))

(def student-scope (scope [:g [1 2 3] :d [0 1] :i [0 1] :s [0 1] :l [0 1]]))


;(def g (gr/digraph [:a :b] [:b :c] [:c :d] [:b :a] [:d :b]))


(def s-net (gr/digraph [:d :g] [:i :g] [:i :s] [:g :l]))

(def d-var (variable :d [0 1]))
(def i-var (variable :i [0 1]))
(def s-var (variable :s [0 1]))
(def g-var (variable :g [1 2 3]))
(def l-var (variable :l [0 1]))

(def mycpds
	{:d (factor [d-var] {[0] 0.6 [1] 0.4})
	 :i (factor [i-var] {[0] 0.7 [1] 0.3})
	 :s (factor [i-var s-var] {[0 0] 0.95 [0 1] 0.05 [1 0] 0.2 [1 1] 0.8})
	 :g (factor [i-var d-var g-var]
			{ [0 0 1] 0.3 [0 0 2] 0.4 [0 0 3] 0.3 
			  [0 1 1] 0.05 [0 1 2] 0.25 [0 1 3] 0.7
			  [1 0 1] 0.9 [1 0 2] 0.08 [1 0 3] 0.02 
			  [1 1 1] 0.5 [1 1 2] 0.3 [1 1 3] 0.2})
	 :l (factor [g-var l-var] 
			{ [1 0] 0.1 [1 1] 0.9 
			  [2 0] 0.4 [2 1] 0.6 
			  [3 0] 0.99 [3 1] 0.01})
	 })
;(def i-o {:d {:inputs [] :outputs [d-var]}
;		  :i {:inputs [] :outputs [i-var]}
;		  :s {:inputs [i-var] :outputs [s-var]}
;		  :g {:inputs [i-var d-var] :outputs [g-var]}
;		  :l {:inputs [g-var] :outputs [l-var]}})

(defn graph-from-cpds [cpds]
	(apply gr/digraph (for [[v f] cpds pv (get-vars (f)) :when (not (= v pv))] [pv v])))
	
(defn bayesian-network 
	([cpds] (bayesian-network (graph-from-cpds cpds) cpds))
	([graph cpds] {:graph graph :cpds cpds}))
	
(defn graph [byn] (:graph byn))
(defn cpds [byn] (:cpds byn))

(defn joint-pd [byn] (reduce multiply (vals (cpds byn))))


(def mybn (bayesian-network s-net mycpds))
(def mybncpd (joint-pd mybn))

 ;(marginalize-factor (marginalize-factor (marginalize-factor (marginalize-factor mybncpd :g) :i) :d) :s)

 
(defn get-3-sublists [l]
	(map (fn [a b c] [a b c]) l (rest l) (rest (rest l))))
 
(defn v-structure? [byn a b c] (subset? #{a c} (gr/incoming (graph byn) b)))
(defn get-v-structures [byn nodes] 
	(filter #(apply v-structure? byn %) (get-3-sublists nodes)))

(defn observed? [inputs variable] (not (nil? (inputs variable))))
(defn v-structure-activated? [byn inputs variable] (let [v-and-descendants 	(gr-al/bf-traverse (graph byn) variable)]
	(some true? (map #(observed? inputs %) v-and-descendants))))

(defn active-trail? 
	([byn nodes] (= (count (get-v-structures byn nodes)) 0))
	([byn inputs nodes] 
		(let [v-struct-nodes (map second (get-v-structures byn nodes))
			  non-v-struct-nodes (difference (set nodes) (set v-struct-nodes))]
			(and 
				(every? true? (map #(not (observed? inputs %)) non-v-struct-nodes))
				(every? true? (map #(v-structure-activated? byn inputs %) v-struct-nodes))))))

(defn equal-scopes? [s1 s2] (= (set s1) (set s2)))

(defn equal-factors? [f1 f2]
	(if (equal-scopes? (f1) (f2))
		(every? true? (for [i (scope-space (f1))] (== (f1 i) (f2 i))))
		false))

;; TODO independent? numeric problems... rounding errors appear
(defn independent? [pd v1 v2]
	(let [pd12 (reduce #(marginalize-factor %1 %2) pd (remove #(or (= % v1) (= % v2)) (get-vars (pd))))
		  pd1 (marginalize-factor pd12 v2)
		  pd2 (marginalize-factor pd12 v1)]
			(println (factor-table pd1))
			(println (factor-table pd2))
			(println (factor-table pd12))
			(println (factor-table (multiply pd1 pd2)))
			(equal-factors? pd12 (multiply pd1 pd2))))



(defn- edge? [graph a b] (or (gr/has-edge? graph a b) (gr/has-edge? graph b a)))
(defn- path? [graph nodes] (every? true? (map #(edge? graph %1 %2) nodes (rest nodes))))

;; TODO horribly inefficient brute-force
(defn get-graph-paths [dag x y]
	(let [nodes (remove #(or (= % x) (= % y)) (gr/nodes dag))]
		(filter #(path? dag %)
		(map #(concat [x] % [y]) (apply concat (for [i (range 0 (inc (count nodes)))] (apply concat (map comb/permutations (comb/combinations nodes i))))))
		)
		))
		
(defn- d-separated-brute? [byn inputs x y]
	(every? false? (map #(active-trail? byn inputs %) (get-graph-paths (graph byn) x y))))

(defn- d-separated-non-descendants?
	[byn inputs x y]
	(if (every? true? (map #(observed? inputs %) (gr/incoming (graph byn) x)))
		(not (nil? (some #{y} (difference (gr/nodes (graph byn)) (set (gr-al/bf-traverse (graph byn) x))))))
		false))
	
(defn d-separated? [byn inputs x y]
	(or (d-separated-non-descendants? byn inputs x y) (d-separated-brute? byn inputs x y)))
	
(def extended-student-net (gr/digraph [:c :d] [:d :g] [:i :g] [:i :s] [:g :l] [:g :h] [:s :j] [:l :j] [:j :h]))
(def testbn (bayesian-network extended-student-net {}))

;; TODO implement the factorizes method
(defn factorizes? ([] true) ([pd graph] true))
(defn conditionally-independent? [byn inputs v1 v2] (and (factorizes?) (d-separated? (byn inputs v1 v2))))


;; TODO it's the other way around - if i-map then factorizes
(defn i-map? [pd graph] (factorizes? pd graph))


(defn random-dag
	[n]
	(let [nodes (shuffle (map #(keyword (str "n" %)) (range n)))
		  m (int (round (sqrt n)))
		  i (inc (rand-int (/ n m)))]
		(loop [ this-level (take i nodes)
				remaining (drop i nodes)
				relations {}]
				
				(if (empty? remaining)
					(gr/digraph relations)
						
					(let [j (inc (rand-int (/ n m)))
						  next-level (take j remaining)
						  new-rels (reduce into relations (flatten (map (fn [t] {t (take (inc (rand-int (count next-level))) (shuffle next-level))}) this-level)))
						  ]
						(recur next-level (drop j remaining) new-rels))
					)
				)
		)
	)
(defn rand-node-pair [dag] (take 2 (shuffle (gr/nodes dag))))

(defn test-get-paths [n] (let [g (random-dag n)] (apply get-graph-paths g (rand-node-pair g))))