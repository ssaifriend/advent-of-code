(ns aoc2018.d3
  (:require [clojure.string :as s]
            [util]))

(defn split [line]
  (let [re (re-matches #"#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)" line)
        [_ idx x y width height] re]
    (when (not (nil? re))
      {:idx    (Integer/parseInt idx)
       :x      (Integer/parseInt x)
       :y      (Integer/parseInt y)
       :width  (Integer/parseInt width)
       :height (Integer/parseInt height)})))

(defn parse []
  (->>
    (util/read-file "2018/2018.3.input")
    (s/split-lines)
    (map split)))

(defn inc-part-1 [arr cord]
  (let [{:keys [x y width height]} cord]
    (doseq [x (->> (range x Integer/MAX_VALUE) (take width))
            y (->> (range y Integer/MAX_VALUE) (take height))]
      (aset-int arr x y (inc (aget arr x y)))))
  arr)

(defn preprocess-part-1 [cords]
  (let [sum (fn [cols] (->> cords (map #(apply + ((apply juxt cols) %)))))
        max-width (->> (sum [:x :width]) (apply max))
        max-height (->> (sum [:y :height]) (apply max))]
    (reduce inc-part-1
            (make-array Integer/TYPE max-width max-height)
            cords)))

(defn aggregate-part-1 [arrs]
  (->> arrs
       (mapcat vec)
       (filter #(>= % 2))))

(comment
  (->> (parse)
       (preprocess-part-1)
       (aggregate-part-1)
       (count))

  (let [cords (parse)
        sum (fn [cols] (->> cords (map #(apply + ((conj cols juxt) %)))))]
    (sum '(:x :width)))
  (let [cord (split "#2 @ 124,968: 28x19")
        a ((apply juxt [:x :width]) cord)]
    (println a))
  (let [a (make-array Integer/TYPE 10 10)]
    (aset-int a 9 0 (inc (aget a 9 0)))
    (println (mapcat vec a))
    (seq (flatten a))))
