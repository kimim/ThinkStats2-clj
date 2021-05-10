(ns thinkstats2-clj.chart
  (:import
   (org.knowm.xchart XYChart
                     CategoryChart
                     PieChart
                     VectorGraphicsEncoder
                     VectorGraphicsEncoder$VectorGraphicsFormat)))

(defn histogram
  "Plot a histogram chart"
  [xs ys & {:keys [filename title legend width height]
            :or {title "histogram" legend "y(x)" width 500 height 400}}]
  (let [xy (CategoryChart. width height)]
    (.setTitle xy title)
    (.addSeries xy legend xs ys)
    (VectorGraphicsEncoder/saveVectorGraphic
     xy filename VectorGraphicsEncoder$VectorGraphicsFormat/SVG)))
