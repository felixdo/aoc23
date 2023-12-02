(ns aoc23.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            )
  )

(defn read-input [file]
  (-> (slurp (io/resource file))
      (clojure.string/split-lines)))

(defonce input (read-input "day2.txt"))
(defonce cube-re #"(\d+) (green|red|blue)")
(defonce line-re #"Game (\d+):(.+)")

(defn id-if-possible [line]
  "returns the game id if the game is possible, 0 otherwise"
  (let [[_ game-id rolls] (re-matches line-re line)
        roll-seq (re-seq cube-re rolls)
        pred (fn [{:strs [red green blue]}]
               (or
                (> (or red 0) 12)
                (> (or green 0) 13)
                (> (or blue 0) 14)))]
    (if (some pred (->> roll-seq (map (fn [[_ cubes color]] {color (Integer/parseInt cubes)}))))
      0
      (Integer/parseInt game-id)
      ))
  )

(defn min-pos [result next]
  "reduction function that keeps track of the minimum required cubes for each color"
  (let [color (first (first next))]
    (update result color (fn [current-min] (max current-min (get next color))))))

(defn cube-power [line]
  "calculate the power of the minimum cubes required for a line"
  (let [[_ _ rolls] (re-matches line-re line)
        roll-seq (re-seq cube-re rolls)]
    (->> roll-seq
         (map (fn [[_ cubes color]] {color (Integer/parseInt cubes)}))
         (reduce min-pos {"red" 0 "green" 0 "blue" 0})
         vals
         (reduce *)
         )
    )
  )

"Answers part 1:" (reduce + (map id-if-possible input))
"Answers part 2:" (reduce + (map cube-power input))
