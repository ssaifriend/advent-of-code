(ns aoc2020.d2
  (:require [clojure.string :as s]
            [util]))

(defn extract [s]
  (let [rem (re-find #"([0-9]+)-([0-9]+) ([a-z]+): ([a-z]+)" s)
        [_ first second char password] rem]
    (if (nil? rem)
      (throw (Exception. "not parsed"))
      {:first    (Integer/parseInt first)
       :second   (Integer/parseInt second)
       :char     char
       :password password})))

(defn parse []
  (->>
    (util/read-file "2020/2020.2.input")
    (s/trim)
    (s/split-lines)
    (map #(extract %))))

(defn ->preprocess-part-1 [hms]
  (->> hms
       (map
         #(assoc %
            :frequencies
            (->> (s/split (:password %) #"")
                 (frequencies))))))

(defn ->aggregate-part-1 [hms]
  (->> hms
       (filter
         #(let [{:keys [first second frequencies char]} %]
            (when-let [f (get frequencies char)]
              (and (>= f first)
                   (<= f second)))))
       (count)))


(defn- part-2-valid-password? [hm]
  (let [{:keys [first second password char]} hm
        password (s/split password #"")]
    (->> [first second]
         (map dec)
         (map #(nth password %))
         (filter #(= char %))
         (count)
         (= 1))))

(defn ->preprocess->aggregate-part-2 [hms]
  (->> hms
       (filter part-2-valid-password?)
       (count)))


(comment
  (->>
    (parse)
    (->preprocess-part-1)
    (->aggregate-part-1)
    (println))
  (->>
    (parse)
    (->preprocess->aggregate-part-2)
    (println))
  (println (-> (apply list [13 16]) (conj 'juxt)))
  (println (-> (apply list) (conj [13 16] 'juxt)))
  (let [r (range 20)
        cmd (-> (apply map #('nth r %) [13 16]))]
    (println cmd)))

