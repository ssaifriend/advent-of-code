(ns aoc2020.d1
  (:require [clojure.math.combinatorics :as comb])
  (:require [util]))

(defn parse []
  (->>
    (util/read-file "2020/2020.1.input")
    (util/split-line-to-int)))

(defn ->preprocess [t ints]
  (comb/combinations ints t))

(defn find-sum [v combos]
  (->> combos
       (filter #(= v (apply + %)))))

(defn ->print [seqs]
  (->>
    (first seqs)
    (apply *)))

(comment
  (->>
    (parse)
    (->preprocess 2)
    (find-sum 2020)
    (->print))
  (->>
    (parse)
    (->preprocess 3)
    (find-sum 2020)
    (->print)))
