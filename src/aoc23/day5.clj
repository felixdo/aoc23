(ns aoc23.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            )
  (:gen-class)
  )

(def input (-> (slurp (io/resource "day5.txt"))
                   (clojure.string/split-lines)))

(defn elf-map [phase dest-start source-start map-range x]
  (let [source-end (+ source-start map-range)]
    (if (<= source-start x source-end)
      (+ dest-start
         (- x source-start))
      nil)
      )
  )

(defn parse-seeds-p1 [in]
  (map parse-long (re-seq #"\d+" (first in))))

(defn parse-seeds-p2 [in]
  (let [ranges (->> (re-seq #"\d+" (first in))
                    (map parse-long)
                    (partition 2)
                    (map (fn [[start range]] [start (+ start range)]))
                    )
        ]
    ranges))

(defn optimize-seeds-p2 [in]
(let [all (sort-by first (parse-seeds-p2 in))]
  (loop [result [(first all)]
         todo (rest all)
         ]
    (if (seq todo)
      (recur
       (apply conj
              (pop result)
              (let [[left-start left-end] (peek result)
                    [right-start right-end] (first todo)]
                (if (<= right-start left-end)
                  (if (<= right-end left-end)
                    [left-start left-end]
                    [left-start right-end])
                  (list [left-start left-end] [right-start right-end]))))
       (rest todo))
      result))))       


(defn parse-mappings [in]
    (loop [maps []
           current-map []
           current-phase nil
           todo (rest in)]
      (if-let [line (first todo)]
        (cond
          (some
           #{"seed-to-soil map:"
             "soil-to-fertilizer map:"
             "fertilizer-to-water map:"
             "water-to-light map:"
             "light-to-temperature map:"
             "temperature-to-humidity map:"
             "humidity-to-location map:"} [line])
          (recur (conj maps
                       (apply some-fn (conj current-map identity)))
                 []
                 line
                 (rest todo))
          (empty? line)
          (recur maps
                 current-map
                 current-phase
                 (rest todo))
          :else (let [[_ dest-start source-start map-range] (re-matches #"(\d+) (\d+) (\d+)" line)]
                  (recur maps
                         (conj current-map (partial elf-map current-phase (parse-long dest-start) (parse-long source-start) (parse-long map-range)))
                         current-phase
                         (rest todo))))
        (apply comp (reverse (conj maps (apply some-fn (conj current-map identity)))))
        )))


(comment 
"Part 1:"
(let [seeds (parse-seeds-p1 input)
      mappings (parse-mappings input)]
  (reduce min (pmap mappings seeds))
  ))


(defn -main [args]
  (prn "result"
   (let [seeds (optimize-seeds-p2 input)
         mappings (parse-mappings input)
         ]
     (pmap 
          (fn [x]
            (loop [m Integer/MAX_VALUE
                   todo (range (first x) #_(+ 10 (first x)) (second x))
                   done 0
                   ]
              (if-let [c (first todo)]
                (do
                  (when (= 0 (mod done 1000000))
                    (prn \.))
                  (recur
                   (min m (mappings c))
                   (rest todo)
                   (inc done)
                   ))
                m))) seeds ))))


(comment (-main "hoho"))