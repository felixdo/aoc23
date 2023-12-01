(ns aoc23.aoc23
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            )
  )

(comment "Day 1 - Part 1"
         (defn coords-p1 [line]
           (let [digits (filter #(Character/isDigit %1) line)
                 first (* 10 (- (int (first digits)) 48))
                 last (- (int (last digits)) 48)
                 ]
             (+ first last)))
         (defn day1part1 [file]
           (let [ lines (-> (slurp (io/resource file))
                            (clojure.string/split-lines))]
             (reduce + (map coords-p1 lines))
             ))
         (day1part1 "day1_part1.txt")
)

(comment "Day 1 - Part 2"
         (defn coords [line]
           (loop [l line
                  tens nil
                  ones nil
                  ]
             (if (seq l)
               (let [digit (condp = (first l)
                             \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9
                             (condp (fn [x y] (.startsWith y x)) l
                               "one" 1 "two" 2 "three" 3 "four" 4 "five" 5
                               "six" 6 "seven" 7 "eight" 8 "nine" 9
                               nil))
                     newtens (or tens digit)
                     newones (or digit ones)]
                 (recur
                  (apply str (rest l)) ;; TODO avoid reconverting to str as improvement
                  newtens
                  newones))
               (+ (* 10 (or tens 0))
                  (or ones 0)))))

         "Test the example"
         (reduce + (map coords
                        ["two1nine"
                         "eightwothree"
                         "abcone2threexyz"
                         "xtwone3four"
                         "4nineeightseven2"
                         "zoneight234"
                         "7pqrstsixteen"]))

         (defn day1part2 [file]
           (let [lines (-> (slurp (io/resource file))
                           (clojure.string/split-lines)
                           )]
             (reduce + (map coords lines))
             ))
         (day1part2 "day1_part1.txt") ;; same input as part1

)