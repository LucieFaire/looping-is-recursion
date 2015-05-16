(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc b e] 
  	    (if (zero? e)
  	    	acc
  	    	(recur (* acc b) b (dec e))))]
  (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
  	    	(first a-seq)
  	    	(recur (rest a-seq))))	

(defn seq= [seq1 seq2]
  (let [helper (fn [acc s1 s2]
  	    (cond 
  	    	(and (empty? s1) (empty? s2))
  	    	acc
            (or
            (or (empty? s1) (empty? s2))
            (not (= (first s1) (first s2)))) 
  	    	false
  	    	:else (recur true (rest s1) (rest s2))))]
  (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [acc 0
  	     p pred
  	     s a-seq]
  	(if (empty? s)
  	 nil     
  	(if (p (first s)) 
  		acc
  		(recur (inc acc) p (rest s))))))

(defn avg [a-seq]
  (loop [acc 0
  	     sum 0
  	     s a-seq]
  	     (if (empty? s)
  	     	(/ sum acc)
  	     	(recur (inc acc) (+ sum (first s)) (rest s)))))

(defn parity [a-seq]
 (loop [s a-seq 
 	    a #{}]
    (cond
      (empty? s)
        a
      (contains? a (first s))
        (recur (rest s) (disj a (first s)))
      :else
        (recur (rest s) (conj a (first s))))))

(defn fast-fibo [n]
  (loop [n n
  	     f1 1
  	     f2 0]
  	     (if (< n 1)
  	     	f2
  	     	(recur (dec n) f2 (+ f1 f2)))))

(defn cut-at-repetition [a-seq]
  (loop [s a-seq
  	     a []]
  	     (cond 
  	     	(empty? s) a
  	     	(contains? (set a) (first s)) a
  	     	:else
  	     	  (recur (rest s) (conj a (first s))))))

	