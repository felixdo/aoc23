(ns aoc23.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :refer [ceil floor]]
            )
  (:gen-class)
  )

(def input (->> (slurp (io/resource "day11.txt"))
               (clojure.string/split-lines)
               (mapv #(into [] (seq %)))
               )
)

(defn galaxy? [c]
  (= \# c))

(defn expand [in expand-fn]
  (let [transpose #(apply mapv vector %)]
    (-> in
        expand-fn
        transpose
        expand-fn
        transpose))
  )

(defn repeat-dot-rows
  "repeat input rows that are only dots"
  [in]
  (loop [i in
         o []]
    (if (seq i)
      (let [f (first i)]
        (if (every? #(= \. %) f)
          (recur (rest i) (conj o f f))
          (recur (rest i) (conj o f))))
      o))
  )

(defn millions-of-galaxies
  "if row is only dots, replace the dots with \e for expanded"
  [in]
  (loop [i in
         o []]
    (if (seq i)
      (let [f (first i)]
        (if (every? #(not (galaxy? %)) f)
          (recur (rest i) (conj o (apply vector (repeat (count f) \e))))
          (recur (rest i) (conj o f))))
      o)))

(defn galaxies
  "Find all galaxy pairs, includes dupes so divide the result by 2 later..."
  [in]
  (for [x (range (count (first in)))
        y (range (count in))
        :let [c (-> in (get y) (get x))]
        :when (= \# c)]
    [x y]))

(defn manhattan-distance
  "Standard manhattan distance between two galaxies"
  [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

(defn expanded-universe-distance
  "Walk from one galaxy to the next and count 1 for each step,
   but walking over 'e' is actually 1 million steps"
  [[x1 y1] [x2 y2] input]
  (let [distfn (fn [v]
                 (reduce (fn [result c]
                           (+ result
                              (condp = c
                                \. 1
                                \# 1
                                \e 1000000
                                ))) 0 v))
        x-start (min x1 x2)
        x-end (max x1 x2)
        y-start (min y1 y2)
        y-end (max y1 y2)
        row (get input y-start)
        col (mapv #(get % x-end) input)
        ]
    (+ (distfn (subvec row (inc x-start) (inc x-end)))
       (distfn (subvec col (inc y-start) (inc y-end))))
    )
)

(comment "Part 1"
(let [g (-> input
            (expand repeat-dot-rows)
            galaxies)
      dists (for [g1 g g2 g]
              (manhattan-distance g1 g2))]
  (-> (reduce + dists)
      (/ 2)))
)


(comment "Part 2"
(let [universe (-> input
                  (expand millions-of-galaxies))
      g (galaxies universe)
      dists (for [g1 g g2 g]
              (expanded-universe-distance g1 g2 universe))]
  (-> (reduce + dists)
      (/ 2)))
)
