(ns aoc2022.d2
  (:require [util :refer [read-file]]
            [clojure.string :as s]
            [medley.core :as m]))

(defn get-read-file []
  (-> "2022/2022.2.input" read-file s/split-lines))

(defn ->text-to-rps [s]
  (case s
    "A" :rock
    "B" :paper
    "C" :scissors
    "X" :rock
    "Y" :paper
    "Z" :scissors))

(defn parse-p1 [ss]
  (map (fn [s]
         (->> (s/split s #" ")
              (map ->text-to-rps)))
       ss))

(defn ->text-to-rps-and-result [s]
  (case s
    "A" :rock
    "B" :paper
    "C" :scissors
    "X" :lose
    "Y" :draw
    "Z" :win))

(defn parse-p2 [ss]
  (map (fn [s]
         (->> (s/split s #" ")
              (map ->text-to-rps-and-result)))
       ss))

(def point-map
  {:rock {:rock :draw :paper :win :scissors :lose}
   :paper {:rock :lose :paper :draw :scissors :win}
   :scissors {:rock :win :paper :lose :scissors :draw}})

(def choose-rps
  (m/map-vals (fn [mv]
                (zipmap (vals mv)
                        (keys mv)))
              point-map))

(def shape-point
  {:rock 1 :paper 2 :scissors 3})

(def result-point
  {:win 6 :draw 3 :lose 0})

(defn p1 [games]
  (->> games
       (map (fn [[player me]]
              (+ (result-point (get-in point-map [player me]))
                 (shape-point me))))
       (apply +)))

(defn p2 [games]
  (->> games
       (map (fn [[player result]]
              (+ (shape-point (get-in choose-rps [player result]))
                 (result-point result))))
       (apply +)))

(comment
  (def sample "A Y
B X
C Z")

  (def parse-sample (-> sample s/split-lines))
  (-> parse-sample
      parse-p1
      p1)
  (-> (get-read-file) parse-p1 p1)
  (-> parse-sample
      parse-p2
      p2)
  :rcf)