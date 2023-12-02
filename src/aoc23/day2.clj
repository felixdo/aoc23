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

(defn parse-line
  "turns line into [game-id [rolls]]"
  [line]
  (let [[_ game-id rolls] (re-matches line-re line)
        roll-seq (re-seq cube-re rolls)]
    [(Integer/parseInt game-id)
     ;; transducers: https://clojure.org/reference/transducers#_into
     (into []
           (comp
            (map rest)
            (map (fn [[count color]] { (keyword color) (Integer/parseInt count) })))
           roll-seq)]))

(defn id-if-possible
  "evaluates to the game-id if the game is possible, 0 otherwise"
  [[game-id rolls]]
  (let [impossible? (fn [{:keys [red green blue] :or {red 0 green 0 blue 0}}]
                      (or (> red 12)
                          (> green 13)
                          (> blue 14)))]
    (if (some impossible? rolls)
      0
      game-id)))

(defn cube-power
  "calculate the power of the minimum cubes required for a parsed line"
  [[_ rolls]]
  (->> rolls
       (reduce (fn [result next]
                 (let [[color] (keys next)]
                   (update result color #(max % (color next)))))
               {:red 0 :green 0 :blue 0})
       vals
       (reduce *)
       )
  )

"Answers part 1:" (reduce + (->> input (map (comp id-if-possible parse-line))))
"Answers part 2:" (reduce + (->> input (map (comp cube-power parse-line))))
