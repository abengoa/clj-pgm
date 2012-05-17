(ns pgm.test.hmm
	(use pgm.hmm clojure.test))
	
(def mh (markov-chain {:1 {:2 0.2 :3 0.8} :2 {:2 0.3 :3 0.3 :4 0.4} :3 {:1 0.1 :4 0.9} :4 {:4 0.2 :1 0.3 :2 0.4 :3 0.1}}))
(def mhh (hmm {:1 {:2 0.2 :3 0.8} :2 {:2 0.3 :3 0.3 :4 0.4} :3 {:1 0.1 :4 0.9} :4 {:4 0.2 :1 0.3 :2 0.4 :3 0.1}}
			  {:1 {:a 0.9 :b 0.05 :c 0.05} :2 {:a 0.1 :b 0.4 :c 0.5} :3 {:a 0.3 :b 0.3 :c 0.4} :4 {:a 0.6 :b 0.3 :c 0.1}}))

(def rs (hmm {:rainy {:sunny 0.3 :rainy 0.7} :sunny {:sunny 0.6 :rainy 0.4}}
			{:rainy {:walk 0.1 :shop 0.4 :clean 0.5} :sunny {:walk 0.6 :shop 0.3 :clean 0.1}}))

(def genome (hmm {"H" {"H" 0.5 "L" 0.5} "L" {"H" 0.4 "L" 0.6}} {"H" {"A" 0.2 "C" 0.3 "G" 0.3 "T" 0.2} "L" {"A" 0.3 "C" 0.2 "G" 0.2 "T" 0.3}}))
(def gene-observations "GGCACTGAA")

(deftest viterbi-test
	(is '(:sunny :rainy :rainy) (viterbi rs {:rainy 0.6 :sunny 0.4} [:walk :shop :clean]))
	(is "HHHLLLLLL" (apply str (viterbi genome {"H" 0.5 "L" 0.5} (map str (seq gene-observations))))))

	; (def z (hmm {:1 {:1 1.0}} {:1 {:a 0.2 :b 0.8}}))
	; (reduce (fn [m k] (update-in m [k] inc))  {:a 0 :b 0} (take 100000 (generate z :1)))
	; (reduce (partial merge-with +)  {:a 0 :b 0} (map (fn [x] {x 1}) (take 100000 (generate z :1))))
	; (reduce (fn [m k] (update-in m [k] inc))  {:a 0 :b 0 :c 0} (take 100000 (generate (hmm {:1 {:1 1.0}} {:1 {:a 0.2 :b 0.1 :c 0.7}}) :1)))