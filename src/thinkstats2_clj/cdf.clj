(ns thinkstats2-clj.cdf)

(defn percentile-rank [scores your-score]
  (* 100.0
     (/ (count (filter #(<= % your-score) scores))
        (count scores))))

(defn percentile [scores percentile-rank]
  (let [index (* percentile-rank
                 (/ (dec (count scores)) 100))]
    (nth (sort scores) index)))

(defn cdf-prob [samples x]
  (/ (count (filter #(<= % x) samples))
     (count samples)))

(defn cdf-value [samples p]
  (let [index (* p (dec (count samples)))]
    (nth (sort samples) index)))

(defn random [samples]
  (let [index (rand-int 101)]
    (percentile samples index)))

(defn sample [samples n]
  (let [indices (take n (repeatedly #(rand-int 101)))]
    (map #(percentile samples %) indices)))
