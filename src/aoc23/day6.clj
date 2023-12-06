(ns aoc23.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :refer [ceil floor]]
            )
  (:gen-class)
  )

(def input (-> (slurp (io/resource "day6.txt"))
               (clojure.string/split-lines)))

(defn parse-line-p1 [line]
  (->> line
       (re-seq #"(\d+)")
       (map first)
       (map parse-long)))

(defn parse-line-p2 [line]
  (->> line
       (re-seq #"(\d+)")
       (map first)
       (reduce str)
       parse-long
       ))

;;; part 1
(reduce * (loop [times (parse-line-p1 (first input))
                 distances (parse-line-p1 (second input))
                 results []
                 ]
            (if-let [t (first times)]
              (let [d (first distances)]
                (let [winning-combos
                      (->> (for [x (range t)]
                             (> (* (- t x) x) d))
                           (filter true?)
                           count)
                      ]
                  (recur (rest times)
                         (rest distances)
                         (conj results winning-combos))))
              results)))


(defn winning? [waiting-time race-duration record]
  (> (* (- race-duration waiting-time)
        waiting-time)
     record))

(defn min-winning [race-duration record]
  (let [p-down (fn [l p] (+ l (floor (/ (- p l) 2))))
        p-up (fn [p r] (+ p (floor (/ (- r p ) 2))))]
    (loop [l 0
           p (p-down 0 race-duration)
           r race-duration]
      (if (winning? p race-duration record)
        (recur l (p-down l p) p)
        (if (winning? (+ 1 p) race-duration record)
          (int (+ 1 (- race-duration (* 2 (+ 1 p)))))
          (recur p (p-up p r) r))))))

(comment "part 2"
(apply min-winning
       (let [time (parse-line-p2 (first input))
             distance (parse-line-p2 (second input))]
         [time distance]))
)