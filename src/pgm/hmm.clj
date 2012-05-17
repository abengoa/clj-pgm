(ns pgm.hmm)

;;=========== Markov chains 
(defn markov-chain [probs] {:type :markov-chain :probs probs})

(defn select-rnd-element 
	"Selects a random element from a probability map, where the keys are the possible elements and the values are the probabilities of selecting each element"
	[prob-map] 
	(let [rn (rand)] (first (reduce (fn [[a p] [n prob]] (if (and (<= p rn) (>= (+ p prob) rn)) [n (+ p prob)] [a (+ p prob)])) [nil 0] (shuffle (seq prob-map))))))

;; Generate an infinite sequence of elements from a Markov model.
(defmulti generate :type)
(defmethod generate :markov-chain
	[mc start]
	(concat [start] (lazy-seq (generate mc (select-rnd-element ((:probs mc) start))))))

;;=========== Hidden Markov Models	
(defn hmm 
	"Builds a hidden Markov model from the map of transition probabilities (for each state as a key, the value is a map with states as keys and transition
	probabilities as values) and emission probabilities map (for each state as a key, the value is a map with observed values as keys and emission probabilities
	as values)."
	[transitions observations] 
	{:type :hmm :transitions transitions :observations observations})
	
(defn states 
	"Return the set of states of a HMM"
	[hmm] 
	(set (keys (:transitions hmm))))
	
(defn observations 
	"Get the set of possible observed values of a HMM"
	[hmm] 
	(set (apply concat (map keys (vals (:observations hmm))))))

(defmethod generate :hmm [hmm start]
	(concat [(select-rnd-element ((:observations hmm) start))] (lazy-seq (generate hmm (select-rnd-element ((:transitions hmm) start))))))
	
(defn argmax 
	"Given a function and a list, returns a vector [a b] where a is the element of the list that maximizes the function, and b is (f a)"
	[f l] (reduce (fn ([] nil) ([x] x) ([a b] (let [[x1 f1] a [x2 f2] b] (if (> f1 f2) a b)))) (map (fn [x] [x (f x)]) l)))
	
(defn argmax-list 
	"Given a function and a list, returns a list of vectors [a b] where a are the elements of the list that maximize the function, and b is (f a)"
	[f l]
	(first (partition-by second (reverse (sort-by second (map (fn [x] [x (f x)]) l))))))

(defn- viterbi-fwd 
	"Forward iteration of the Viterbi algorithm. Receives the list of states, the transition probability map, the emission probability map, 
	the remainder of the observed sequence and the map with the previous most probable paths. Returns a list of the most probable paths for
	the rest of observations."
	[sts trans obs obs-seq mt]
	(if (empty? obs-seq)
		[mt]
		 (let [nmt  (reduce into {} (map (fn [j]  {j  (let [[[fs & r] v]  (argmax (fn [[s [x t]]]
		 (* t ((trans s) j) ((obs j) (first obs-seq)))) mt)] [fs v])}) sts))]
			(concat [mt] (viterbi-fwd sts trans obs (rest obs-seq) nmt)))))

(defn- viterbi-bck 
	"Backwards iteration of the Viterbi algorithm. Builds the list of most probable states from the list of most probable paths."
	[current-state from-state mts]
	(if (empty? mts)
		[current-state]
		(let [[sf p] ((first mts) current-state)]
		(concat (viterbi-bck from-state sf (rest mts)) [current-state]))))
	
(defn viterbi
	"Implementation of the Viterbi algorithm for hidden Markov models. Given a sequence of observations returns the most probable sequence
	of states that generated the observed values."
	[hmm start-probs observed]
	(let [trans (:transitions hmm)
			 obs (:observations hmm)
			 sts (states hmm)
			 mt (reduce into {} (map (fn [s] {s [:start (* (start-probs s) ((obs s) (first observed)))]}) sts)) ;; initialize the most probable paths for the first observation
			 mts (reverse (viterbi-fwd sts trans obs (rest observed) mt))  ;; build the rest of most probable paths
			 [st [sf p]] (first (reverse (sort-by (fn [[a [b c]]] c) (first mts))))]
		  (viterbi-bck st sf (rest mts)) ;; construct the most probable sequence of states
		  ))
