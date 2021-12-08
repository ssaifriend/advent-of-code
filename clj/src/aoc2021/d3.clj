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

; https://stackoverflow.com/questions/8314789/rotate-a-list-of-list-matrix-in-clojure
(defn transpose [inputs]
  (apply map list inputs))

(defn bit-map-frequencies [inputs]
  (map frequencies inputs))

(defn gamma-rate [bits]
  (if (<= (bits 0) (bits 1)) 1 0))

(defn epsilon-rate [bits]
  (if (<= (bits 0) (bits 1)) 0 1))

(defn gamma-rates [bit-map]
  (map gamma-rate bit-map))

(defn epsilon-rates [bit-map]
  (map epsilon-rate bit-map))

(defn oxygen-bit-criteria [bits]
  (->> bits
       frequencies
       gamma-rate))

(defn co2-bit-criteria [bits]
  (->> bits
       frequencies
       epsilon-rate))

(defn- generate-rating [{:keys [bit-criterias bit-seq f]
                         :or {bit-criterias []}
                         :as m}]
  (let [bits (->> (map first bit-seq)
                  (map #(Integer/parseInt (str %))))
        bit-criteria (f bits)

        bit-seq (->> bit-seq
                     (filter (fn [bits]
                               (= (Integer/parseInt (str (first bits))) bit-criteria)))
                     (map #(s/join "" (rest %))))
        bit-criterias (conj bit-criterias bit-criteria)]

    (if (and (< 1 (count bit-seq))
             (seq (first bit-seq)))
      (recur (assoc m :bit-criterias bit-criterias :bit-seq bit-seq))
      (if (= 1 (count bit-seq))
        (concat bit-criterias (s/split (first bit-seq) #""))
        bit-criterias))))


(comment
  (def sample "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010")
  (let [bit-map (-> sample
                    s/split-lines
                    parse
                    transpose
                    bit-map-frequencies)]
    (*
      (Integer/parseInt (s/join "" (gamma-rates bit-map)) 2)
      (Integer/parseInt (s/join "" (epsilon-rates bit-map)) 2)))
  (let [bit-map (-> (get-read-file) parse transpose bit-map-frequencies)]
    (*
      (Integer/parseInt (s/join "" (gamma-rates bit-map)) 2)
      (Integer/parseInt (s/join "" (epsilon-rates bit-map)) 2)))

  (let [bit-seq (-> sample s/split-lines)
        oxygen-rating (generate-rating {:f #'oxygen-bit-criteria
                                        :bit-seq bit-seq})
        co2-rating (generate-rating {:f #'co2-bit-criteria
                                     :bit-seq bit-seq})]
    (*
      (Integer/parseInt (s/join "" oxygen-rating) 2)
      (Integer/parseInt (s/join "" co2-rating) 2)))
  (let [bit-seq (get-read-file)
        oxygen-rating (generate-rating {:f #'oxygen-bit-criteria
                                        :bit-seq bit-seq})
        co2-rating (generate-rating {:f #'co2-bit-criteria
                                     :bit-seq bit-seq})]
    (*
      (Integer/parseInt (s/join "" oxygen-rating) 2)
      (Integer/parseInt (s/join "" co2-rating) 2))))
