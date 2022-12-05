(ns aoc2022.d4
  (:require [clojure.string :as s]
            [clojure.set :refer [subset? intersection]]
            [util :refer [read-file]]))

(defn get-read-file []
  (-> "2022/2022.4.input" read-file s/split-lines))

(defn number-range-set [start end]
  (set (range (Integer/parseInt start) (inc (Integer/parseInt end)))))

(defn parse-p1 [ss]
  (map (fn [s]
         (let [re (re-find #"([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)" s)
               [_ start1 end1 start2 end2] re]
           [(number-range-set start1 end1)
            (number-range-set start2 end2)]))
       ss))

(defn intersection? [s1 s2]
  (seq (intersection s1 s2)))

(defn aggregate [f]
  (fn [s1 s2]
    (or (f s1 s2)
        (f s2 s1))))

(defn p1 [ss]
  (filter #(apply (aggregate subset?) %)
          ss))

(defn p2 [ss]
  (filter #(apply (aggregate intersection?) %)
          ss))

(comment
  (def sample "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

  (->> (s/split-lines sample)
       parse-p1
       p1
       count)
  (->> (get-read-file) parse-p1 p1 count)
  (->> (s/split-lines sample)
       parse-p1
       p2
       count)
  (->> (get-read-file) parse-p1 p2 count)
  :rcf)