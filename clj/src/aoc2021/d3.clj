(ns aoc2021.d3
  (:require [util :refer [read-file]]
            [clojure.string :as s]))

(defn get-read-file []
  (-> "2021/2021.3.input" read-file s/split-lines))

(defn parse [inputs]
  (map (fn [s]
         (->> (s/split s #"")
              (map #(Integer/parseInt %))))
       inputs))

(defn each-bit-map [inputs]
  ; https://stackoverflow.com/questions/8314789/rotate-a-list-of-list-matrix-in-clojure
  (let [transposed (apply map list inputs)]
    (map #(frequencies %) transposed)))

(defn gamma-rate [bit-map]
  (->> bit-map
       (map (fn [vs]
              (if (< (vs 0) (vs 1)) 1 0)))
       (s/join "")))

(defn epsilon-rate [bit-map]
  (->> bit-map
       (map (fn [vs]
              (if (< (vs 0) (vs 1)) 0 1)))
       (s/join "")))


(comment
  (let [bit-map (-> "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"
                    s/split-lines
                    parse
                    each-bit-map)]
    (*
      (Integer/parseInt (gamma-rate bit-map) 2)
      (Integer/parseInt (epsilon-rate bit-map) 2)))
  (let [bit-map (-> (get-read-file) parse each-bit-map)]
    (*
      (Integer/parseInt (gamma-rate bit-map) 2)
      (Integer/parseInt (epsilon-rate bit-map) 2))))
