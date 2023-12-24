(ns aoc23.day15
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :refer [ceil floor]]
            )
  (:gen-class)
  )

(def input (->> (slurp (io/resource "day15.txt"))
                (s/split-lines)
                (mapcat #(s/split % #","))
                ))

(defn HASH [s]
  (reduce
   (fn [acc c]
     (-> acc
         (+ (int c))
         (* 17)
         (mod 256))) 0 s))

(defn remove-lens [boxes label]
  (let [index (HASH label)
        remover
        (fn [box i]
          (if (= i index)
            (filter (fn [[l f]] (not (= label l))) box)
            box))
        ]
    (map remover boxes (range 0 256))))

(defn insert-lens [boxes [label focus]]
  (let [index (HASH label)
        replacer (fn [box i]
                   (if (= i index)
                     (loop [newbox (list)
                            contents (reverse box)
                            found? false]
                       (if (seq contents)
                         (let [[l f] (first contents)
                               match? (= l label)]
                           (recur
                            (conj newbox 
                                  (if match?
                                    [l focus]
                                    [l f]))
                            (rest contents)
                            (or found? match?)))                           
                         (if found?
                           newbox
                           (conj newbox [label focus]))))
                       box))]
        (map replacer boxes (range 0 256))))

(defn step [boxes instruction]
  (if (.endsWith instruction "-")
    (remove-lens boxes (apply str (butlast instruction)))
    (insert-lens boxes (s/split instruction #"="))))

(defn focal-power [box index]
  (reduce +
          (map
           (fn [i [label focus]]
             (* i (parse-long focus) (inc index)))
           (reverse (range 1 (inc (count box))))
           box)))

(comment "example"
(let [sample (-> (apply list (repeat 256 '()))
                 (step  "rn=1")
                 (step "cm-")
                 (step "qp=3")
                 (step "cm=2")
                 (step "qp-")
                 (step "pc=4")
                 (step "ot=9")
                 (step "ab=5")
                 (step "pc-")
                 (step "pc=6")
                 (step "ot=7"))]
  (reduce + (map focal-power sample (range 0 256))))
)


(comment "part 2"
         (loop [boxes (apply list (repeat 256 '()))
                instructions input]
           (if (seq instructions)
             (recur
              (step boxes (first instructions))
              (rest instructions))
             (reduce + (map focal-power boxes (range 0 256)))
             ))
)
