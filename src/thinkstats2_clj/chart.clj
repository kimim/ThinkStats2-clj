(ns thinkstats2-clj.chart
  (:import
   (org.knowm.xchart XYChart
                     CategoryChart
                     PieChart
                     VectorGraphicsEncoder
                     VectorGraphicsEncoder$VectorGraphicsFormat)
   (java.awt Color)))

(def svg-format VectorGraphicsEncoder$VectorGraphicsFormat/SVG)

(defn normalize
  "Normalize the series data, fill the empty values with 0"
  [series]
  (let [xs (for [serie series]
             (map first (second serie)))
        xs (flatten xs)
        x-max (apply max xs)
        x-min (apply min xs)
        normal-xs (->> (range x-min (inc x-max))
                       (reduce #(into %1 {%2 0}) {}))
        new-series (for [serie series]
                     {(first serie) (into (sorted-map)
                                          (merge normal-xs (second serie)))})]
    (reduce into {} new-series)))

(defn histogram
  "Plot a histogram chart"
  [series & {:keys [filename title width height]
             :or {title "histogram" width 600 height 400}}]
  (let [xy (CategoryChart. width height)]
    (.setTitle xy title)
    (doseq [serie series]
      (.addSeries xy (first serie) (keys (second serie)) (vals (second serie))))
    (VectorGraphicsEncoder/saveVectorGraphic
     xy filename svg-format)))

(defn step
  "Plot a histogram chart"
  [series & {:keys [filename title width height]
             :or {title "histogram" width 600 height 400}}]
  (let [xy (CategoryChart. width height)]
    (.setTitle xy title)
    (.setOverlapped (.getStyler xy) true)
    (doseq [serie series]
      (.setFillColor
       (.addSeries xy (first serie) (keys (second serie)) (vals (second serie)))
       (Color. 0 0 0)))
    (VectorGraphicsEncoder/saveVectorGraphic
     xy filename svg-format)))
