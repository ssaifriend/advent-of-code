(ns aoc2018.d5
  (:require [clojure.string :as s]
            [util]))

(defn parse [f]
  (s/split f #""))

(defn is-lower-case? [s]
  (= s (s/lower-case s)))

(defn reverse-case [s]
  (if (is-lower-case? s) (s/upper-case s) (s/lower-case s)))

(defn reaction [ss]
  (->> ss
       (count)
       (range)
       (reduce (fn [v i]
                 (let [s (nth ss i)
                       n (nth ss (inc i) nil)
                       rs (reverse-case s)]
                   (if (= rs n)
                     (reduced (apply conj v (drop (+ i 2) ss)))
                     (conj v s))))
               [])))

(defn get-reaction-seq [s]
  (lazy-seq (cons s (get-reaction-seq (reaction s)))))

(defn get-finished-reaction [s]
  (loop [c nil
         s s]
    (let [n (first s)]
      (if (= c n)
        (s/join c)
        (recur n
               (rest s))))))

(defn find-p2 [s]
  (let [s (s/join "" s)]
    (->> (range (int \a) (inc (int \z)))
         (map #((comp str char) %))
         (map #(-> s (s/replace % "") (s/replace (s/upper-case %) "")))
         (distinct)
         (map #(->> (parse %)
                    (get-reaction-seq)
                    (get-finished-reaction)
                    (count))))))


(comment
  (def input "dabAcCaCBAcCcaDA")
  (def input (util/read-file "2018/2018.5.input"))
  (->> input (parse) (p1-seq) (take 10) (map s/join))
  (->> input (parse) (get-reaction-seq) (get-finished-reaction) (count))
  (->> input (parse) (find-p2) (apply min))

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