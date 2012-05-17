(ns pgm.test.factor
  (:use [pgm.factor])
  (:use [clojure.test]))

(def x (factor (scope :i [0 1] :d [0 1]) {[0 0] 0.126 [0 1] 0.009 [1 0] 0.252 [1 1] 0.06}))
(x {:i 0 :d 1})
		
(def f1 (factor (scope :a [1 2 3] :b [1 2]) {[1 1] 0.5 [1 2] 0.8 [2 1] 0.1 [2 2] 0 [3 1] 0.3 [3 2] 0.9}))
(def f2 (factor (scope :b [1 2] :c [1 2]) {[1 1] 0.5 [1 2] 0.7 [2 1] 0.1 [2 2] 0.2}))
(def f1f2 (multiply f1 f2))
			
(def mf1f2 (marginalize-factor f1f2 :b))
(mf1f2 {:a 1 :c 1})
(def rf1f2 (reduce-factor f1f2 :c 1))
(def prf1f2 (normalize-factor rf1f2))
  
;(deftest replace-me ;; FIXME: write
;  (is false "No tests have been written."))
