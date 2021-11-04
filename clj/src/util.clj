(ns util
  (:require [clojure.string :as s]))

(defn ->split-line->int
  "line 단위로 분리하고 숫자로 변환합니다."
  [ds]
  (->> (s/split-lines ds)
       (map #(Integer/parseInt %))))

(defn make-input-path [path]
  (str "../input/"
       (if (s/starts-with? path "/") (subs path 1) path)))

(defn read-file [path]
  (-> path make-input-path slurp))
