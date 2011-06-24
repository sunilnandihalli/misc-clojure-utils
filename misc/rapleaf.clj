(ns rapleaf
  (:require [clojure.contrib.lazy-seqs :as ls]))

(defn xor [& ps]
  (loop [[p & rp] ps cur-state nil]
    (cond
     (and cur-state p) nil
     p (recur rp p)
     rp (recur rp cur-state)
     true cur-state)))

(let [p2 (ls/powers-of-2)]
  (defn rapleaf-mad-matrix [[row coll]]
    (loop [[i j] [row coll]
           [p & ps] (reverse (take-while #(<= % (max row coll)) p2))
           cur-val 0]
      (if p (recur (map #(if (>= % p) (- % p) %) [i j])
                   ps (+ cur-val (if (apply xor (map #(< % p) [i j])) p 0))) cur-val))))
