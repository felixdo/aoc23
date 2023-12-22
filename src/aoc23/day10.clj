(ns aoc23.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :refer [ceil floor]]
            )
  (:gen-class)
  )

(def input (-> (slurp (io/resource "day10.txt"))
               (clojure.string/split-lines)))

(comment 
  (def input [
              "....."
              ".S-7."
              ".|.|."
              ".L-J."
              "....."])

  (def input  ["-L|F7"
               "7S-7|"
               "L|7||"
               "-L-J|"
               "L|-JF"])

  (def input 
    ["..F7."
     ".FJ|."
     "SJ.L7"
     "|F--J"
     "LJ..."])
)


(def size (count (first input)))
(def maze (apply str input))
(def start (.indexOf maze "S"))
(def distances (assoc (apply vector (repeat (* size size) -1)) start 0))

(defn east [pos]
  (if (= (- size 1) (mod pos size))
    nil
    (inc pos)))

(defn west [pos]
  (if (= 0 (mod pos size))
    nil
    (dec pos)))

(defn north [pos]
  (if (< pos size)
    nil
    (- pos size)))

(defn south [pos]
  (if (>= pos (* (- size 1) size))
    nil
    (+ pos size)))

(defn pipe [pos]
  (get maze pos))

(defn next-pos [pos]
  (let [n (north pos)
        s (south pos)
        e (east pos)
        w (west pos)
        result (condp = (pipe pos)
                 \| [n s]
                 \- [w e]
                 \L [n e]
                 \J [n w]
                 \7 [s w]
                 \F [s e]
                 \. []
                 \S
                 [n w #_e #_s #_ "ok i cheated here"])]
    (when (some nil? result)
      (throw (Exception. "Ran off...")))
    result
    ))


(defn calc-distances []
  (loop [positions [start]
         dists distances
         steps 0]
    (if (seq positions)
      (let [new-positions (filter #(= -1 (get dists %)) (reduce concat (map next-pos positions)))
            dist-pairs (for [x positions] [x steps])
            ]
        (recur new-positions
               (apply assoc (cons dists (reduce concat dist-pairs)))
               (inc steps)))
      dists)))

(defn out-of-loop []
  (let [dists (calc-distances)
        loop? (fn [pos] (> (get dists pos) 0))
        ]
    (loop [work [0]
           out-of-loop #{}]
      (prn (count work) (first work))
      
      (if (seq work)
        (if (loop? (first work))
          (do
            (prn "Loop!")
            (recur (rest work) out-of-loop))
          (let [more-work
                (->> (list north east south west)
                     (map #(% (first work))) 
                     (filter #(and
                               (not (out-of-loop %))
                               (not (nil? %))))
                     )
                ]
            (if (some #{19600} more-work)
              (prn "Duh!" (first work))
              (recur (concat more-work (rest work))
                     (conj out-of-loop (first work)))
              )))
        out-of-loop
        ))))

(defn print-loop []
  (let [l (map #(if (= -1 %)
                  "."
                  "x")
               (calc-distances))]
    (run! #(println (reduce str %)) (partition size l))))
  
    
(print-loop)
    
  
(comment "part1"
         (reduce max (calc-distances))
)


(comment "part2"
         
         (count (out-of-loop))
         )

size
(* size size)
(map #(% 5) (list north east south west))


