(ns aoc23.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :refer [ceil floor]]
            )
  (:gen-class)
  )

(def input (-> (slurp (io/resource "day8.txt"))
               (clojure.string/split-lines)))


(defn make-dwarf-map [lines]
  (reduce
   (fn [res next]
     (let [[pos l r] (re-seq #"\w\w\w" next)]
       (assoc res pos { \L l \R r })    
       ))
   {} lines))


(def desert (make-dwarf-map (rest (rest input))))
(def directions (cycle (first input)))


(comment
  "Part 1"
  (loop [steps 0
         pos "AAA"
         dirs directions]
    (if (= "ZZZ" pos)
      steps
      (recur (inc steps)
             (get-in desert [pos (first dirs)])
             (rest dirs))))
)


(comment
  (let [ghosts (filter #(.endsWith % "A") (keys desert))
        done? #(.endsWith % "Z")
        ]
    (map
     (fn [ghost]
       (loop [pos ghost
              steps 0
              offset nil
              dirs directions
              ]
         #_(prn pos)
         (if (done? pos)
           (if offset
             {:offset offset :cycle-after (- steps offset)}
             (recur
              (get-in desert [pos (first dirs)])
              (inc steps)
              steps
              (rest dirs)
              ))
           (recur
            (get-in desert [pos (first dirs)])
            (inc steps)
            offset
            (rest dirs)))))
     ghosts
     ))

)
