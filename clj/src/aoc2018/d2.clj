(ns aoc2018.d2
  (:require [clojure.string :as s]
            [util]))

(defn parse []
  (->>
    (util/read-file "2018/2018.2.input")
    (s/split-lines)))

(defn ->process-part-1 [ss]
  (->> ss
       (map #(s/split % #""))
       (map #(frequencies %))))

(defn- find-count [hms c]
  (->> hms
       (map #(vals %))
       (filter (fn [vs] (some #(= % c) vs)))
       (count)))

(defn ->aggregate-part-1 [hms]
  (->> '(2 3)
       (map #(find-count hms %))
       (apply *)))


(defn ->process-part-2 [ss]
  (->> ss (map #(s/split % #""))))

(defn- eq-str [s1 s2]
  (->>
    (map vector s1 s2)
    (filter #(apply = %))
    (map first)))

(defn- eq-strs [ss]
  (let [[s1 & others] ss
        s1-count (count s1)]
    (->> others
         (map #(eq-str s1 %))
         (filter #(= 1 (- s1-count (count %)))))))

(defn ->aggregate-part-2 [ss]
  (if-let [strs (seq (eq-strs ss))]
    (first strs)
    (recur (rest ss))))

(defn ->print-part-2 [ss]
  (->> ss (s/join "") (println)))

(comment
  (->>
    (parse)
    (->process-part-1)
    (->aggregate-part-1)
    (println))
  (->>
    (parse)
    (->process-part-2)
    (->aggregate-part-2)
    (->print-part-2))

  (some #(= (val %) 1) {:a 1 :b 2})
  (eq-str ["a" "b" "d"] ["a" "c" "d"])
  (->> (eq-str ["a"] ["b"]) (seq)))