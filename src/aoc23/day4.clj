(ns aoc23.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            )
  )

(def input (-> (slurp (io/resource "day4.txt"))
                   (clojure.string/split-lines)))

(defn points [line scorefn]
  (let [all (re-seq #"\d+|\|" line)]
  (loop [numbers (rest all)
         card {:id (parse-long (first all)) :winning-numbers #{}} ]
    (let [f (first numbers)
          r (rest numbers)]
      (if f
        (if (= "|" f)
          (recur r (conj card [:points 0]))
          (if (:points card)
            (recur r
                   (if (some (:winning-numbers card) [(parse-long f)])
                     (update card :points scorefn)
                     card))
            (recur r (update card :winning-numbers #(conj % (parse-long f)))))
          )
        card)))))

(defn points-p1 [line] 
  (points line #(if (= 0 %)
                  1
                  (* 2 %))))

(comment
"Part 1 solution: "
(reduce + (map (comp :points points-p1) input))
)

(defn points-p2 [line]
  (points line inc))

(comment 
"Part 2 solution: "
(loop [todo (map points-p2 input)
       counts (reduce #(conj %1 [(+ 1 %2) 1]) {} (range (count input)))]
  (let [current (first todo)
        newcounts (reduce conj
                          counts
                          (for [x (range (+ 1
                                            (:id current))
                                         (min (+ 1
                                                 (:id current)
                                                 (:points current))
                                              (+ 1 (count input))))]
                            [x (+ (get counts x)
                                  (get counts (:id current)))]))]

    (if-let [r (seq (rest todo))]
      (recur r newcounts)
      (reduce + (vals newcounts)))))
)
