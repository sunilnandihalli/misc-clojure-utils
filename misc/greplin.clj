(ns greplin.core
  (:refer-clojure)
  (:require [clojure.contrib.lazy-seqs :as ls]
	    [clojure.contrib.combinatorics :as c]
	    [clojure.contrib.trace :as t]))
;; level 1 challenge
(def inp "FourscoreandsevenyearsagoourfaathersbroughtforthonthiscontainentanewnationconceivedinzLibertyanddedicatedtothepropositionthatallmenarecreatedequalNowweareengagedinagreahtcivilwartestingwhetherthatnaptionoranynartionsoconceivedandsodedicatedcanlongendureWeareqmetonagreatbattlefiemldoftzhatwarWehavecometodedicpateaportionofthatfieldasafinalrestingplaceforthosewhoheregavetheirlivesthatthatnationmightliveItisaltogetherfangandproperthatweshoulddothisButinalargersensewecannotdedicatewecannotconsecratewecannothallowthisgroundThebravelmenlivinganddeadwhostruggledherehaveconsecrateditfaraboveourpoorponwertoaddordetractTgheworldadswfilllittlenotlenorlongrememberwhatwesayherebutitcanneverforgetwhattheydidhereItisforusthelivingrathertobededicatedheretotheulnfinishedworkwhichtheywhofoughtherehavethusfarsonoblyadvancedItisratherforustobeherededicatedtothegreattdafskremainingbeforeusthatfromthesehonoreddeadwetakeincreaseddevotiontothatcauseforwhichtheygavethelastpfullmeasureofdevotionthatweherehighlyresolvethatthesedeadshallnothavediedinvainthatthisnationunsderGodshallhaveanewbirthoffreedomandthatgovernmentofthepeoplebythepeopleforthepeopleshallnotperishfromtheearth")
(def inp-ints (vec (map (fn [x] (->> x int (#(cond 
					      (>= % 97) (- % 97)
					      true (- % 65)))))
			(to-array inp))))

(def n-inp-ints (count inp-ints))

(defn palindrome? [begin end]		;begin and end inclusive
  (loop [cur-b begin
	 cur-e end]
    (cond
     (>= cur-b cur-e) true
     (= (inp-ints cur-b) (inp-ints cur-e)) (recur (inc cur-b) (dec cur-e)))))
      
(defn longest-palindrome-starting-from
  [start]
  (loop [cur-b start
	 cur-e (dec n-inp-ints)]
    (cond
     (palindrome? cur-b cur-e) [cur-b cur-e]
     (< cur-b cur-e) (recur cur-b (dec cur-e)))))

(defn longest-palindrome []
  (loop [cur-longest [0 0]
	 cur-longest-length 1
	 cur-start 0]
    (if	(< (- n-inp-ints cur-start) cur-longest-length)
      cur-longest
      (let [[new-start new-end :as new-longest] (longest-palindrome-starting-from cur-start)
	    new-longest-length (inc (- new-end new-start))]
	(if (> new-longest-length cur-longest-length)
	  (recur new-longest new-longest-length (inc cur-start))
	  (recur cur-longest cur-longest-length (inc cur-start)))))))

(defn greplin-l1 []
  (let [[start end] (longest-palindrome)]
    (take (inc (- end start)) (drop start (into-array inp)))))

#_(greplin-l1)
;; ans = (\r \a \n \y \n \a \r)

;; level 2 challenge

(defn common-elems [s1 s2]
  (let [first-common-element (fn [p q]
			       (loop [[fp & rp :as wp] p [fq & rq :as wq] q]
				 (cond
				  (= fp fq) [rp rq fp]
				  (< fp fq) (recur rp wq)
				  (> fp fq) (recur wp rq))))
	[rest-of-s1 rest-of-s2 first-common-elem] (first-common-element s1 s2)]
    (cons first-common-elem (lazy-seq (common-elems rest-of-s1 rest-of-s2)))))

(defn factorize [s]
  (filter #(= 0 (mod s %)) (take-while #(< % s) ls/primes)))
  
(defn greplin-l2 []
  (->> (common-elems (ls/fibs) ls/primes)
       (drop-while #(< % 227000))
       first inc factorize
       (apply +)))

#_(greplin-l2)

;; ans = 352 

;; level 3 challenge

(def array-of-ints [3, 4, 9, 14, 15, 19, 28, 37, 47, 50, 54, 56, 59, 61, 70, 73, 78, 81, 92, 95, 97, 99])

(defn count-leaves [tr]
  (count (filter #(= '() %) (tree-seq seq? seq tr))))
  
(defn binary-search [coll val]
  (letfn [(helper [start end]
	    (let [mid (int (/ (+ start end) 2))]
	      (if (> (- end start) 1)
		(if (<= (coll mid) val)
		  (recur mid end)
		  (recur start mid))
		[start end])))]
    (helper -1 (count coll))))

(defn filter-tree-with-entries-above [s max-entry]
  (filter (fn [x] (every? #(< % max-entry) (flatten x))) s))
	  
(defn create-sums-map [m sum]
  (cond
   (contains? m sum) m
   (< sum 0) m
   (= sum 0) (assoc m 0 '(()))
   true (let [[start end :as interval] (binary-search array-of-ints sum)]
	  (if (< start 0)
	    (assoc m sum nil)
	    (loop [cur start m1 m possible-sets '()]
	      (let [current-entry (array-of-ints cur) 
		    delta (- sum current-entry)
		    m2 (create-sums-map m1 delta)
		    set-summing-to-delta (m2 delta)
		    valid-set-summing-to-delta (filter-tree-with-entries-above set-summing-to-delta current-entry)
		    new-possible-sets (if (seq valid-set-summing-to-delta)
					(cons (cons current-entry valid-set-summing-to-delta) possible-sets)
					possible-sets)]
		(if (> cur 0)
		  (recur (dec cur) m2 new-possible-sets)
		  (assoc m2 sum new-possible-sets))))))))
   
(defn greplin-l3 []
  (let [fm (reduce create-sums-map {} array-of-ints)]
    (apply + (vals (into {} (for [x array-of-ints]
			      [x (dec (count-leaves (fm x)))]))))))



#_(time (greplin-l3))

;; ans 179

(defn angle-between-clock-handles [hours minutes seconds]
  (let [seconds-angle (* seconds 6)
        minutes-angle (+ (* minutes 6) (/ seconds-angle 60.0))
        hours-angle (+ (* hours 30) (/ minutes-angle 12))]
    (- hours-angle minutes-angle)))

#_(angle-between-clock-handles 3 0 0)
#_(angle-between-clock-handles 3 33 10)

(defn toggle-doors [door-state round-id] ;door-id is index+1
  (map-indexed (fn [id cur-door-state]
                 (if (= 0 (rem (inc id) round-id))
                   (not cur-door-state) cur-door-state))
               door-state))

#_(keep #(if (second %)
           (inc (first %)))
        (map-indexed #(vector %1 %2)
                     (reduce toggle-doors
                             (repeat 100 false) (range 1 101))))

#_(defn evenly-balanced-teams [coll]
    (let [n (count coll)
          [n1 n2] (if (odd? n)
                    (let [tmp (/ (dec n) 2)]
                      [tmp (inc tmp)])
                    (repeat 2 (/ n 2)))]))

(defvar primes
  (concat 
   [2 3 5 7]
   (lazy-seq
    (let [primes-from
	  (fn primes-from [n [f & r]]
	    (if (some #(zero? (rem n %))
		      (take-while #(<= (* % %) n) primes))
	      (recur (+ n f) r)
	      (lazy-seq (cons n (primes-from (+ n f) r)))))
	  wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
			6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
			2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel))))
  "Lazy sequence of all the prime numbers.")