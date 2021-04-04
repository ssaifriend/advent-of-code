(ns util
  (:require [clojure.string :as s]))

(defn split-line-to-int
  "line 단위로 분리하고 숫자로 변환합니다."
  [ds]
  (->> (s/split-lines ds)
       (map #(Integer/parseInt %))))

(defn input-path [path]
  (->>
    (if (s/starts-with? path "/") (subs path 1) path)
    (str "../input/")))

(defn read-file [path]
  (->> path
       (input-path)
       (slurp)))
