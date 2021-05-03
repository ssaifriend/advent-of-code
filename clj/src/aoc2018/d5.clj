(ns aoc2018.d5
  (:require [clojure.string :as s]
            [util]))

(defn parse [f]
  (s/split f #""))

(defn generate-around [s]
  ;(->> (->> s (count) (range) (map dec))
  ;     (map (fn [i] (apply #(get s %) (->> (range i) (take 3)))))
  ;     (println))
  (->> (->> s (count) (inc) (range 1) (map dec))
       (map (fn [i]
              (apply list [(->> s (take (inc i)) (drop i) (first))
                           (->> s (take i) (drop (dec i)) (first))
                           (->> s (take (+ i 2)) (drop (inc i)) (first))])))))

(defn around-equal? [[c & others]]
  (->> others
       (remove nil?)
       (map s/lower-case)
       (filter #(= (s/lower-case c) %))
       (empty?)))

(defn process-p1 [s]
  (->> s
       (generate-around)
       (filter around-equal?)
       (map first)))

(defn p1-seq [s]
  (lazy-seq (cons s (p1-seq (process-p1 s)))))


(comment
  (def input "dabAcCaCBAcCcaDA")
  (def input (util/read-file "2018/2018.5.input"))
  (->> input (parse) (generate-around) (println))
  (->> input (parse) (p1-seq) (take 5) (map s/join))
  )