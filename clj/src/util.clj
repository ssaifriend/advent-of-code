(ns util
  (:require [clojure.string :as s]))

(defn ->split-line->int
  "line 단위로 분리하고 숫자로 변환합니다."
  [ds]
  (->> (s/split-lines ds)
       (map #(Integer/parseInt %))))

(defn ->split-line-group->int
  "빈 line으로 한번 분리하고, 분리한 그룹을 line 단위로 분리한 뒤, 숫자로 변환합니다"
  [ds]
  (->> (s/split ds #"\n\n")
       (map ->split-line->int)))

(defn make-input-path [path]
  (str "../input/"
       (if (s/starts-with? path "/") (subs path 1) path)))

(defn read-file [path]
  (-> path make-input-path slurp))

(comment
  (-> "" ->split-line->int)
  (-> "1
2

3
4" ->split-line-group->int)
  :rcf)