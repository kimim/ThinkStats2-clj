(ns thinkstats2-clj.pmf)


(defn probabilities [coll]
  (let [n (count coll)]
    (->> coll
         frequencies
         (map #(vec [(first %) (/ (second %) n)]))
         (into {}))))

(defn normalize [probmap]
  (let [sum (->> probmap
                 (map second)
                 (reduce +))]
    (reduce #(update %1 %2 / sum) probmap (keys probmap))))
