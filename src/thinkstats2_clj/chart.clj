(ns thinkstats2-clj.chart
  (:import
   (org.knowm.xchart XYChart
                     CategoryChart
                     PieChart
                     VectorGraphicsEncoder
                     VectorGraphicsEncoder$VectorGraphicsFormat
                     CategorySeries$CategorySeriesRenderStyle
                     XYSeries$XYSeriesRenderStyle)
   (org.knowm.xchart.style.markers SeriesMarkers)
   (org.knowm.xchart.style Styler$LegendPosition)
   (java.awt Color))
  (:require [thinkstats2-clj.cdf :as cdf]))

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
    (doseq [serie (normalize series)]
      (.addSeries xy (first serie) (keys (second serie)) (vals (second serie))))
    (VectorGraphicsEncoder/saveVectorGraphic
     xy filename svg-format)))

(defn step
  "Plot a stepped bar chart"
  ([series & {:keys [filename title width height]
              :or {title "step diagram" width 600 height 400}}]
   (let [xy (CategoryChart. width height)]
     (.setTitle xy title)
     (.setOverlapped (.getStyler xy) true)
     (doseq [serie series]
       (let [ss (.addSeries xy (first serie) (keys (second serie)) (vals (second serie)))]
         (.setChartCategorySeriesRenderStyle ss CategorySeries$CategorySeriesRenderStyle/SteppedBar)
        ;; make fill colour transparent
         (.setFillColor ss (Color. 0 0 0 0))))
     (VectorGraphicsEncoder/saveVectorGraphic
      xy filename svg-format)))
  ([{:keys [file series title width height]
     :or {title "XY Chart" width 600 height 400}}]
   (let [xy (XYChart. width height)]
     (.setTitle xy title)
     (.setDefaultSeriesRenderStyle (.getStyler xy) XYSeries$XYSeriesRenderStyle/Step)
     (doseq [serie series]
       (let [ss (.addSeries xy (first serie) (first (second serie))
                            (second (second serie)))]
         (.setMarker ss SeriesMarkers/NONE)))
     (VectorGraphicsEncoder/saveVectorGraphic
      xy file svg-format))))

(defn plot
  "Plot x y chart"
  [{:keys [file series title width height]
    :or {title "XY Chart" width 600 height 400}}]
  (let [xy (XYChart. width height)]
    (.setTitle xy title)
    (.setLegendPosition (.getStyler xy) Styler$LegendPosition/InsideNW);
    (doseq [serie series]
      (let [ss (.addSeries xy (str (first serie)) (first (second serie)) (second (second serie)))]
        (.setFillColor ss (Color. 0 0 0 0))
        (.setMarker ss SeriesMarkers/NONE)))
    (VectorGraphicsEncoder/saveVectorGraphic
     xy file svg-format)))
