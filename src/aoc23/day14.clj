(ns aoc23.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :refer [ceil floor]]
            )
  (:gen-class)
  )

(def line-input (-> (slurp (io/resource "day14.txt"))
               (clojure.string/split-lines)))

(def input (apply vector (mapcat identity line-input)))
(def dim 100)

(defn get-platform-at [platform x y]
  (get platform
       (+ x
          (* dim y)))
  )

(defn column [platform i]
  (for [y (range dim)]
    (get-platform-at platform i y)
    )
  )

(defn columns [platform]
  (for [i (range dim)]
    (column platform i)))

(defn row [platform i]
  (for [x (range dim)]
    (get-platform-at platform x i)))

(defn rows [platform]
  (partition dim platform))

(defn colweight [column]
  (reduce + (map (fn [weight value]
                   (condp = value
                     \O weight
                     0)
                   )
               (reverse (range 1 (inc (count column))))
               column
               )))

(defn platform-weight [platform]
  (reduce + (map colweight (columns platform))))

(defn boulder-or-empty? [x]
  (or (= \. x)
      (= \O x)))

(defn blocked? [x]
  (= \# x))

(defn tilt [v north-or-west?]
  (loop [result []
         work v]
    (if (seq work)
      (let [splitfn
            (condp = (first work)
              \. boulder-or-empty?
              \O boulder-or-empty?
              \# blocked?
              )
            [next-seq todo] (split-with splitfn work)
            ]
        (recur
         (conj result next-seq)
         todo
         ))
      (apply concat (map #((if north-or-west? reverse identity) (sort %)) result)))))

(defn tilt-north [platform]
  (into [] (apply mapcat vector (map #(tilt % true) (columns platform)))))

(defn tilt-west [platform]
  (into [] (mapcat #(tilt % true) (rows platform))))

(defn tilt-south [platform]
  (into [] (apply mapcat vector (map #(tilt % false) (columns platform)))))

(defn tilt-east [platform]
  (into [] (mapcat #(tilt % false) (rows platform))))

(defn cycle [platform]
  (-> platform
      tilt-north
      tilt-west
      tilt-south
      tilt-east
      ))

(comment
  "Part 1, just tilt north and read the platform weight"
  (-> input
      tilt-north
      platform-weight
      )
)

(comment
  "Part 2, cycle until back at some previous state,
   then you get the repeat cycle modulus and can skip a whole bunch of cycles"
  (let [[remaining platform]
        (loop
            [seen {}
             cycles 0
             remaining 1000000000
             p input]
          (if-let [z (get seen p)]
            [(mod remaining (- cycles z)) p]
            (let [newp (cycle p)]
              (recur
               (assoc seen p cycles)
               (inc cycles)
               (dec remaining)
               newp))))]
    (->> (iterate cycle platform)
         (take (inc remaining))
         last
         platform-weight
        ))
)
