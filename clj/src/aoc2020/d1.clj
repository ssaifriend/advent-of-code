(ns aoc2020.d1
  (:require [clojure.math.combinatorics :as comb])
  (:require [util :refer [read-file ->split-line->int]]))

(defn parse []
  (-> "2020/2020.1.input" read-file ->split-line->int))

(defn ->preprocess [t ints]
  (comb/combinations ints t))

(defn find-sum [v combos]
  (filter #(= v (apply + %)) combos))

(defn ->print [seqs]
  (->> (first seqs)
       (apply *)))

(comment
  (->> (parse) (->preprocess 2) (find-sum 2020) ->print)
  (->> (parse) (->preprocess 3) (find-sum 2020) ->print))
