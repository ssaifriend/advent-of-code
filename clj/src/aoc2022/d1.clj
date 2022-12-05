(ns aoc2022.d1
  (:require [util :refer [read-file ->split-line-group->int]]))

(defn get-read-file []
  (-> "2022/2022.1.input" read-file ->split-line-group->int))

(defn sum-of-calories [number-seqs]
  (apply + number-seqs))

(defn p1 [group-number-seqs]
  (->> group-number-seqs
       (map sum-of-calories)
       (apply max)))

(defn p2 [group-number-seqs]
  (->> group-number-seqs
       (map sum-of-calories)
       (sort >)
       (take 3)
       (apply +)))

(comment
  (p1 (get-read-file)) ;; p1
  (p2 (get-read-file)) ;; p2

  :rcf)
