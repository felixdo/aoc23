(ns aoc23.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            )
  )


(def input (-> (slurp (io/resource "day3.txt"))
                   (clojure.string/split-lines)))

#_(def input
  ["467..114.."
   "...*......"
   "..35..633."
   "......#..."
   "617*......"
   ".....+.58."
   "..592....."
   "......755."
   "...$.*...."
   ".664.598.."])

(def cols (count (first input)))
(def rows (count input))

(def top (apply str (repeat (+ 2 cols) \.)))
(def bot top)

;; frame the board in dots to avoid corner cases
(def board
  (into [] (concat [top]
          (mapv #(str "." % ".") input)
          [bot])))


(comment board)

(defn num? [c]
  (<= (int \0) (int c) (int \9)))

(defn sym?
  "A character is a symbol if it's neither a number nor a dot"
  [c]
  (not
   (or
    (num? c)
    (= \. c))))

(defn char-at [board x y]
  (let [row (get board y)]
    (get row x)))

(defn touches-symbol? [board x y]
  (let [up (char-at board x (dec y))
        dn (char-at board x (inc y))
        le (char-at board (dec x) y)
        ri (char-at board (inc x) y)
        ul (char-at board (dec x) (dec y))
        ur (char-at board (inc x) (dec y))
        dl (char-at board (dec x) (inc y))
        dr (char-at board (inc x) (inc y))
        ]
    #_(prn up dn le ri ul ur dl dr)
    (some sym? [up dn le ri ul ur dl dr])))

(char-at board 3 1)
(touches-symbol? board 3 1)

(defn part-no
  [board x y]
  (let [cand (for [i (range x (inc cols))
                   :let [c (char-at board i y)]
                   :while (num? c)]
               {:digit c
                :touches? (touches-symbol? board i y)})
        ]
    (if (some #(:touches? %) cand)
      (->> cand
           (map :digit)
           (apply str)
           Integer/parseInt
           #_(apply str (map :digit cand)))
      0
    )))

(defn scan-p1 []
  (for [y (range 1 (inc rows))
        x (range 1 (inc cols))
        :when (and
               (num? (char-at board x y))
               (not (num? (char-at board (dec x) y))))]
    (part-no board x y))
)

"Answers part 1"
(reduce + (scan-p1))


(defn around [board x y]
  (let [up [x (dec y)]
        dn [x (inc y)]
        le [(dec x) y]
        ri [(inc x) y]
        ul [(dec x) (dec y)]
        ur [(inc x) (dec y)]
        dl [(dec x) (inc y)]
        dr [(inc x) (inc y)]
        ]
    [up dn le ri ul ur dl dr]))

(defn gear-ratio-start [board x y]
  (if (num? (char-at board x y))
    (loop [pos (dec x)]
      (if (< pos 0)
        [0 y]
        (if (num? (char-at board pos y))
          (recur (dec pos))
          [(inc pos) y])))
    nil
    )
  )

(defn part-no2
  [board x y]
  (let [cand (for [i (range x (inc cols))
                   :let [c (char-at board i y)]
                   :while (num? c)]
               c)]
    (Integer/parseInt (apply str cand))))

(defn scan-p2 []
  (for [y (range 1 (inc rows))
        x (range 1 (inc cols))
        :when (= \* (char-at board x y))]

    (->> (around board x y)
         (map (fn [[ax ay]] (gear-ratio-start board ax ay)))
         (filter some?)
         (into #{})

    )))

"Answer for part 2"
(let [two-connected (filter #(= 2 (count %)) (scan-p2))]
  (reduce +
          (map (fn [s]
                 (let [r1 (apply part-no2 board (first s))
                       r2 (apply part-no2 board (second s))
                       ]
                   (* r1 r2)))
               two-connected)))
