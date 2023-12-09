(ns aoc23.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :refer [ceil floor]]
            )
  (:gen-class)
  )

(def input (->> (slurp (io/resource "day9.txt"))
               (clojure.string/split-lines)
               (map #(map parse-long (re-seq #"-?\d+" %)))
               )
)

(defn extrapolate-last [series]
  (if (every? zero? series)
    0
    (+ (last series)
       (extrapolate-last (differences series)))))

(defn extrapolate-first [series]
  (if (every? zero? series)
    0
    (- (first series) (extrapolate-first (differences series)))))

(defn differences [series]
  (for [[s1 s2] (partition 2 1 series)]
    (- s2 s1)))

(comment "part 1"
(->> input
     (map extrapolate-last)
     (reduce +))
)

(comment "part 2"
(->> input
     (map extrapolate-first)
     (reduce +))
)
