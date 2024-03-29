(ns aoc2018.d5
  (:require [clojure.string :as s]
            [util]))

(defn parse [f]
  (s/split f #""))

(defn first-char [s]
  (-> s (str) (first)))

(defn lower-case? [s]
  (->> [\a (first-char s) \z]
       (map int)
       (apply <=)))

(defn reverse-case [s]
  (if (lower-case? s)
    (s/upper-case s)
    (s/lower-case s)))

(defn reaction? [s1 s2]
  (if (lower-case? s1)
    (= (reverse-case s1) s2)
    (= s1 (reverse-case s2))))

(defn reaction [ss]
  (reduce (fn [stack s]
            (if-let [top (peek stack)]
              (if (reaction? top s)
                (pop stack)
                (conj stack s))
              (conj stack s)))
          []
          ss))

(defn match-str? [input s]
  (or (= input s)
      (= input (s/upper-case s))))

(defn find-p2 [ss]
  (->> (range (int \a) (inc (int \z)))
       (map #((comp str char) %))
       (map (fn [s] (remove #(match-str? % s) ss)))
       distinct
       (map #(->> % reaction count))))


(comment
  (def input "dabAcCaCBAcCcaDA")
  (def input (util/read-file "2018/2018.5.input"))
  (->> input parse reaction s/join)
  (->> input parse reaction count)
  (->> input parse find-p2 (apply min))

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

  (defn count-reaction [s]
    (loop [c (first s)
           s s
           cnt 0]
      (if (= c (first s))
        (recur (reverse-case c)
               (rest s)
               (inc cnt))
        cnt)))
  )