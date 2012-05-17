(ns pgm.factor
	(:require [clojure.math.combinatorics :as comb]))
	
(defn variable 
	[name values] {:name name :values values})
(defn get-var-name [v] (:name v))
(defn scope [& var-defs] (map #(apply variable %) (partition 2 var-defs)))
(defn get-vars [scope] (map :name scope))
(defn get-values [scope var-name] (:values (first (filter #(= (:name %) var-name) scope))))

(defn factor [scope table] 
	(fn 
		([] scope) 
		([inputs] (table (vec (map inputs (get-vars scope)))))))

(defn join-scopes [s1 s2]
	(vec (set (concat s1 s2))))
	
(defn multiply [f1 f2]
	(fn
		([] (join-scopes (f1) (f2)))
		([inputs] (* (f1 inputs) (f2 inputs)))))
	
(defn scope-space [s] (map #(apply merge %) (apply comb/cartesian-product (for [v (get-vars s)] (for [val (get-values s v)] {v val})))))

(defn factor-table [f] (map (fn [s] [s (f s)]) (scope-space (f))))

(defn marginalize-factor [f v] 
	(fn 
		([] (remove #(= v (get-var-name %)) (f)))
		([inputs] 
			(let [marginalized-vals (get-values (f) v)]
				(reduce + (for [mv marginalized-vals] (f (into inputs {v mv}))))))))

(defn reduce-factor [f variable value]
	(fn
		([] (remove #(= variable (get-var-name %)) (f)))
		([inputs] (f (into inputs {variable value})))))
		
(defn total-factor-sum [f]
	(reduce + (map f (scope-space (f)))))
	
(defn normalize-factor [f]
	(fn
		([] (f))
		([inputs] (let [total (total-factor-sum f)] (/ (f inputs) total)))))

;; TODO add ways to mark a factor as a conditional probability f = p(x|y) (maybe mark variables as inputs and outputs?
;; TODO once done, add a function to get several factors in and calculate the joint probability distribution. For example, if given the factors [p(i) p(d) (g|i,d) p(s)] and the variables [i,d,g] it will calculate the joint probability distribution by multiplying the required factors (in this case p(i) p(d) and p(g|i,d))