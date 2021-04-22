(ns aoc2018.d3
  (:require [clojure.string :as s]
            [clojure.math.combinatorics :as comb]
            [clojure.set :as set]
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

(defn preprocess [cords]
  (->> cords
       (reduce (fn [hm cord]
                 (let [{:keys [idx x y width height]} cord
                       x (->> (range x Integer/MAX_VALUE) (take width))
                       y (->> (range y Integer/MAX_VALUE) (take height))]
                   (->> (comb/cartesian-product x y)
                        (reduce (fn [hm [x y]]
                                  (let [k (keyword (str x "-" y))]
                                    (update hm k #(if (nil? %)
                                                    [idx]
                                                    (conj % idx)))))
                                hm))))
               {})))

(defn aggregate-part-1 [hm]
  (->> hm
       (map (fn [[_ v]] (count v)))
       (filter #(>= % 2))))

(defn aggregate-part-2 [cords hm]
  (let [overlap-idxs (->> hm
                          (filter (fn [[_ v]] (>= (count v) 2)))
                          (vals)
                          (flatten)
                          (distinct)
                          (set))
        idxs (->> (map :idx cords) (set))]
    (set/difference idxs overlap-idxs)))

(comment
  (->> (parse)
       (preprocess)
       (aggregate-part-1)
       (count))
  (let [cords (parse)]
    (->> cords
         (preprocess)
         (aggregate-part-2 cords)
         (println)))

  (let [cords (parse)
        sum (fn [cols] (->> cords (map #(apply + ((conj cols juxt) %)))))]
    (sum '(:x :width)))
  (let [cord (split "#2 @ 124,968: 28x19")
        a ((apply juxt [:x :width]) cord)]
    (println a))
  (let [a (make-array Integer/TYPE 10 10)]
    (aset-int a 9 0 (inc (aget a 9 0)))
    (println (mapcat vec a))
    (seq (flatten a)))
  (let [sum (fn [cols] (->> cords (map #(apply + ((apply juxt cols) %)))))
        max-width (->> (sum [:x :width]) (apply max))
        max-height (->> (sum [:y :height]) (apply max))
        count (count cords)]))
