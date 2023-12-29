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


(defn inbound [p] (not= \x (get input p)))

(defn beam [start]
  (loop [seen #{}
         beams [start]]
    (if-let [b (first beams)]
      (if (seen b)
        (recur seen (rest beams))
        (recur
         (conj seen b)
         (apply conj (rest beams) ((:dir-fn b) b))
         ))
      (count (into #{} (filter inbound (map :pos seen)))))))

(declare left right up down)

(defn left [{:keys [pos] :as beam} ]
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

(defn up [{:keys [pos] :as beam}]
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

(defn down [{:keys [pos] :as beam} ]
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



(defn right [{:keys [pos] :as beam}]
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

(defn starter-beams-left-side []
  (for [row (range 1 (- dim 1))]
    {:pos (+ 1 (* dim row)) :dir-fn right}))

(defn starter-beams-right-side []
  (for [row (range 1 (- dim 1))]
    {:pos (+ (* dim row) (- dim 2)) :dir-fn left}))

(defn starter-beams-top-side []
  (for [col (range 1 (- dim 1))]
    {:pos (+ dim col) :dir-fn down}))

(defn starter-beams-bottom-side []
  (for [col (range 1 (- dim 1))]
    {:pos (+ (* dim (- dim 2)) col) :dir-fn up}))

(comment "visual check if we got the starting positions correct...")

(defn draw-board [dim input]
  (doseq
      [row (partition dim input)]
    (println (apply str row)))
)

(draw-board dim (apply str (apply assoc (apply vector input) (flatten (map (fn [beam] [(:pos beam) \<]) (starter-beams-right-side))))))

(draw-board dim (apply str (apply assoc (apply vector input) (flatten (map (fn [beam] [(:pos beam) \>]) (starter-beams-left-side))))))

(draw-board dim (apply str (apply assoc (apply vector input) (flatten (map (fn [beam] [(:pos beam) \v]) (starter-beams-top-side))))))

(draw-board dim (apply str (apply assoc (apply vector input) (flatten (map (fn [beam] [(:pos beam) \^]) (starter-beams-bottom-side))))))

)

(comment "part 1"
         (beam {:pos 113 :dir-fn right})
         )

(comment "part 2"
(apply max (pmap beam
                 (concat (starter-beams-left-side)
                         (starter-beams-right-side)
                         (starter-beams-top-side)
                         (starter-beams-bottom-side))
                 ))
)
         



