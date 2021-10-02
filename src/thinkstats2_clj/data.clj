(ns thinkstats2-clj.data
  (:require [clj-http.client :as client]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [tech.v3.dataset :as ds]
            [tablecloth.api :as tablecloth]
            [clojure.string :as str]
            [thinkstats2-clj.cdf :as cdf]
            [thinkstats2-clj.chart :as chart])
  (:import java.util.zip.GZIPInputStream))

(defn fetch-data! [url]
  (let [req (client/get url {:as :byte-array :throw-exceptions false})]
    (if (= (:status req) 200)
      (-> (:body req)
          (io/copy (io/file (last (str/split url #"/"))))))))
