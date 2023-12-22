(ns aoc23.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :refer [ceil floor]]
            )
  (:gen-class)
  )

(defn transpose [m]
  (map #(reduce str %) (apply mapv vector m)))

(defn load [input]
  (loop [result '([])
         lines input]
    (if (seq lines)
      (recur 
       (if (seq (first lines))
         (conj (rest result) (conj (first result) (first lines)))
         (conj result []))
       (rest lines))
      (reverse result)
      )))

(def input (-> (slurp (io/resource "day13.txt"))
               (clojure.string/split-lines)
               load))

(defn find-repetitions [puzzle]
  (loop [line 1
         p puzzle
         m {}
         ]
    (if (seq p)
      (recur (inc line)
             (rest p)
             (update m (first p) #(conj % line)))
      (into #{} (filter #(< 1 (count %)) (vals m)))
      )             
    ))

(defn same? [repetitions [x y]]
  (some #(and
          (some #{x} %)
          (some #{y} %)) repetitions))

(defn find-mirror-start [input]
  (let [reps (find-repetitions input)
        bounds (reverse (some (fn [r]
                                (and
                                 (= 1 (last r))
                                 r)
                                ) reps))]
    (if (seq bounds)
      (first (for [top [1]
            bottom (rest bounds)
            :let [pairs (pairs-surrounded-by bottom top)]
            :when (and (not (nil? pairs))
                       (every? #(same? reps %) pairs))
            ]
               {:reps reps
         :pairs pairs
         :top top
         :bottom bottom
         :centerline (/ bottom 2)
         :puzzle input
         :length (count input)
         }))
      nil
      )
    ))

(defn find-mirror-end [input]
  (if-let [result
           (find-mirror-start (reverse input))]
    (-> result
        (update :centerline #(- (count input) %))
        (update :puzzle #(reverse %)))))

(defn pairs-surrounded-by [upper lower]
  (if (even? (- upper (inc lower)))
    (let [upwards (range (inc lower) upper)
          downwards (reverse upwards)]
      (filter #(< (first %) (second %))
              (map (fn [u d] (list u d))
                   upwards downwards)))
    nil
    ))

(defn solve [puzzle]
  (or
   (when-let [top (->> puzzle
                       (find-mirror-start)
                       :centerline)]
     (* 100 top)
     )
   (when-let [bot (->> puzzle
                       find-mirror-end
                       :centerline)]
     (* 100 bot)
     )
   (->> puzzle
        transpose
        find-mirror-start
        :centerline)
   (->> puzzle
        transpose
        find-mirror-end
        :centerline)))


(defn debugsolve [puzzle]
  [(when-let [top (->> puzzle
                       (find-mirror-start)
                       :centerline)]
     (* 100 top)
     )
   (when-let [bot (->> puzzle
                       find-mirror-end
                       :centerline)]
     (* 100 bot)
     )
   (->> puzzle
        transpose
        find-mirror-start
        :centerline)
   (->> puzzle
        transpose
        find-mirror-end
        :centerline)]
  )


(comment "Part 1"
         (reduce + (map solve input))
         #_(map debugsolve input)
         )

;; look how easy it is... to flip one character in a vector of strings!
(defn smudge [puzzle row col]
  (loop [result []
         r 0
         p puzzle
         ]
    (if (seq p)
      (recur
       (conj result
             (if (= r row)
               (apply str (update (->> (first p)
                                       seq
                                       (apply vector))
                                  col (fn [x]
                                        (condp = x
                                          \. \#
                                          \# \.))))
               
               (first p)
               ))
       (inc r)
       (rest p))
      result)))

(defn all-smudges [puzzle]
  (for [row (range (count puzzle))
        col (range (count (first puzzle)))
        ]
    (smudge puzzle row col)
    ))

(defn solve-smudges [input]
  (let [original-solution (solve input)
        new-solution (->> (all-smudges input)
                           (mapcat debugsolve)
                           (some #(and
                                   (not= % original-solution)
                                   %
                                   ))
                           )
        ]
    new-solution
    )
  )


(comment "Part 2"
 (reduce + (map solve-smudges input))
)
