(ns aoc23.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :refer [ceil floor]]
            )
  (:gen-class)
  )

(def input (-> (slurp (io/resource "day12.txt"))
               (clojure.string/split-lines)
))

(defn broken-seq [config]
  (loop [broken-seq '(0)
         c config]
    (if (seq c)
      (recur 
       (condp = (first c)
         \. (conj broken-seq 0)
         \# (apply list (inc (first broken-seq)) (rest broken-seq)))
       (rest c))
      (filter #(not (= 0 %)) (reverse broken-seq))
      )))

(defn check [line]
  (let [token (s/split line #"\s|,")
        pattern (-> token first seq)
        constraint (map parse-long (rest token))
        combos (loop [c [[]]
                      p pattern
                      ]
                 (if (seq p)
                   (recur
                    (reduce (fn [result x]
                              (apply conj result
                                     (if (= \? (first p))
                                       [ (conj x \#) (conj x \.) ]
                                       [ (conj x (first p)) ])))
                            []
                            c)
                    (rest p)
                    )
                   c
                   ))]
    (count (filter #(= (broken-seq %) constraint) combos))))

(reduce + (map check input))
