(ns thinkstats2-clj.nsfg
  (:require [clj-http.client :as client]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [tech.v3.dataset :as ds]
            [tablecloth.api :as api])
  (:import java.util.zip.GZIPInputStream))

(def dict-line-rx #"^\s+_column\((\d+)\)\s+(\S+)\s+(\S+)\s+%(\d+)(\S)\s+\"([^\"]+)\"")

(defn parse-dict-line
  [line]
  (try (let [[_ col type name f-len f-spec descr] (re-find dict-line-rx line)]
         {:col    (dec (Integer/parseInt col))
          :type   type
          :name   (str/replace name "_" "-")
          :f-len  (Integer/parseInt f-len)
          :f-spec f-spec
          :descr  descr})
       (catch Exception e (println line))))


(defn read-dict-defn
  "Read a Stata dictionary file, return a vector of column definitions."
  [path]
  (with-open [r (io/reader path)]
    (mapv parse-dict-line (butlast (rest (line-seq r))))))


(defn parse-value
  [type raw-value]
  (when (not (empty? raw-value))
    (case type
      ("str12")          raw-value
      ("byte" "int")     (Long/parseLong raw-value)
      ("float" "double") (Double/parseDouble raw-value))))

(defn make-row-parser
  "Parse a row from a Stata data file according to the specification in `dict`.
   Return a vector of columns."
  [dict]
  (fn [row]
    (reduce (fn [accum {:keys [col type name f-len]}]
              (let [raw-value (str/trim (subs row col (+ col f-len)))]
                (conj accum (parse-value type raw-value))))
            []
            dict)))

(defn reader
  "Open path with io/reader; coerce to a GZIPInputStream if suffix is .gz"
  [path]
  (if (.endsWith path ".gz")
    (io/reader (GZIPInputStream. (io/input-stream path)))
    (io/reader path)))

(defn read-dct-data
  "Parse lines from `rdr` according to the specification in `dict`.
   Return a lazy sequence of parsed rows."
  [dict rdr]
  (let [parse-fn (make-row-parser dict)]
    (map parse-fn (line-seq rdr))))

(defn as-dataset
  "Read Stata data set, return an Incanter dataset."
  [dict-path data-path]
  (let [dict   (read-dict-defn dict-path)
        header (map (comp keyword :name) dict)]
    (with-open [r (reader data-path)]
      (api/dataset (read-dct-data dict r)
                   {:layout :as-rows
                    :column-names header
                    :dataset-name "2002FemPreg"}))))
