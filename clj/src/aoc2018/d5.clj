(ns aoc2018.d5
  (:require [clojure.string :as s]
            [util]))

(defn parse [f]
  (s/split f #""))

(defn first-char [s]
  (->> s (str) (char-array) (first)))

(defn is-lower-case? [s]
  (<= (int \a) (int (first-char s)) (int \z)))

(defn reverse-case [s]
  (if (is-lower-case? s) (s/upper-case s) (s/lower-case s)))

(defn make-next-char [ss]
  (let [ss2 (->> ss (drop 1) (reverse) (cons nil) (reverse))]
    (map list ss ss2)))

(defn char= [s1 s2]
  (if s2
    (= (int (first-char s1)) (int (first-char s2)))
    false))

(defn reaction [ss]
  (->> ss
       (make-next-char)
       (reduce (fn [l [s n]]
                 (if (char= (reverse-case s) n)
                   (->> ss
                        (drop (+ (count l) 2))
                        (apply conj l)
                        (reduced))
                   (conj l s)))
               ())
       (reverse)))

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
  (->> input (parse) (get-reaction-seq) (take 5) (map s/join))
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