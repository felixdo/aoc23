(ns aoc23.day16
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :refer [ceil floor]]
            [clojure.core.match :refer [match]]
            )
  (:gen-class)
  )

(def example
[
".|...\\...."
"|.-.\\....."
".....|-..."
"........|."
".........."
".........\\"
"..../.\\\\.."
".-.-/..|.."
".|....-|.\\"
"..//.|...."
 ] 
  )

;; wrap into a frame of x-es and cheat again with the dimension
(def dim #_12 112)
(def input
  (str 
   (apply str (repeat dim \x))
   (->> (slurp (io/resource "day16.txt"))
        (s/split-lines)
        #_example
        (map #(str "x" (s/trim-newline %) "x"))
        (apply str))
   (apply str (repeat dim \x))))

#_(get input 13)


#_input

(declare left right up down)

(defn left [{:keys [dim input pos] :as beam} ]
  (case (get input pos)
    (\- \.) [(update beam :pos dec)]
    \| [(-> beam
            (assoc :pos (- pos dim))
            (assoc :dir-fn up))
        (-> beam
            (assoc :pos (+ pos dim))
            (assoc :dir-fn down))]
    \/ [(-> beam
            (assoc :pos (+ pos dim))
            (assoc :dir-fn down))]
    \\ [(-> beam
            (assoc :pos (- pos dim))
            (assoc :dir-fn up))]
    []
    ))


(defn up [{:keys [dim input pos] :as beam}]
  (case (get input pos)
    (\| \.) [(assoc beam :pos (- pos dim))]
    \- [(-> beam
            (update :pos dec)
            (assoc :dir-fn left))
        (-> beam
            (update :pos inc)
            (assoc :dir-fn right))]
    \\ [(-> beam
            (update :pos dec)
            (assoc :dir-fn left))]
    \/ [(-> beam
            (update :pos inc)
            (assoc :dir-fn right))]
    []
    ))

(defn down [{:keys [dim input pos] :as beam} ]
  (case (get input pos)
    (\| \.) [(assoc beam :pos (+ pos dim))]
    \- [(-> beam
            (update :pos dec)
            (assoc :dir-fn left))
        (-> beam
            (update :pos inc)
            (assoc :dir-fn right))]
    \/ [(-> beam
            (update :pos dec)
            (assoc :dir-fn left))]
    \\ [(-> beam
            (update :pos inc)
            (assoc :dir-fn right))]
    []
    ))



(defn right [{:keys [dim input pos] :as beam}]
  (case (get input pos)
    (\- \.) [(update beam :pos inc)]
    \| [(-> beam
            (assoc :pos (- pos dim))
            (assoc :dir-fn up))
        (-> beam
            (assoc :pos (+ pos dim))
            (assoc :dir-fn down))]
    \/ [(-> beam
            (assoc :pos (- pos dim))
            (assoc :dir-fn up))]
    \\ [(-> beam
            (assoc :pos (+ pos dim))
            (assoc :dir-fn down))]
    []
    )
  )

(defn zap [beam]
  (loop [beams [beam]
         energized #{}
         ]
    (let [new-beams (map #(select-keys % [:pos :dir-fn]) beams)
          new-energized (into energized (map #(select-keys % [:pos :dir-fn]) new-beams))
          ]
      (if (= new-energized energized)
        (count (filter #(not= \x (get input %)) (into #{} (map :pos energized))))
        (recur
         (mapcat #((:dir-fn %) %) beams)
         new-energized
         ))))
  )


(comment "part 1"
  (zap {:dim dim :input input :pos 113 :dir-fn right}))



