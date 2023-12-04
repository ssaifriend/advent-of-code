(ns aoc2023.d2
  (:require [clojure.string :as s]
            [util :refer [read-file]]))

(defn read-file-> []
  (-> "2023/2023.2.input" read-file s/split-lines))

(defn parse-game [s]
  (let [[_ game-id game] (re-matches #"Game ([0-9]+): (.*)" s)]
    {:game-id (Integer/parseInt game-id)
     :games (->> (s/split game #"; ")
                 (map (fn [each-game]
                        (->> (s/split each-game #", ")
                             (map (fn [i-cube]
                                    (let [[i cube-type] (s/split i-cube #" ")]
                                      {:cnt (Integer/parseInt i)
                                       :cube-type (keyword cube-type)})))))))}))

(defn possible? [has-cubes game]
  (->> (:games game)
       (filter (fn [each-game]
                 (->> each-game
                      (filter (fn [{:keys [cnt cube-type]}]
                                (< (has-cubes cube-type) cnt)))
                      not-empty)))
       not-empty))

(defn q1 [ss has-cubes]
  (->> ss
       (map parse-game)
       (remove (partial possible? has-cubes))
       (map :game-id)
       (apply +)))

(defn min-cubes [game]
  (->> (:games game)
       (reduce (fn [acc each-game]
                 (reduce (fn [acc {:keys [cnt cube-type]}]
                           (update acc cube-type #(max % cnt)))
                         acc
                         each-game))
               {:red 0 :green 0 :blue 0})))

(defn q2 [ss]
  (->> ss
       (map parse-game)
       (map min-cubes)
       (map #(->> % vals (apply *)))
       (apply +)))

(comment
  (def sample "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

  (def has-cubes {:red 12 :green 13 :blue 14})
  (q1 (s/split-lines sample) has-cubes)
  (q1 (read-file->) has-cubes)

  (q2 (s/split-lines sample))

  :rcf)