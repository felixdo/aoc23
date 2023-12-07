(ns aoc23.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :refer [ceil floor]]
            )
  (:gen-class)
  )

(def input (-> (slurp (io/resource "day7.txt"))
               (clojure.string/split-lines)))

;; look at which type of card you already have most
;; and add your jokers as that type of card
(defn joker-freqs [hand]
  (let [f (frequencies hand)]
    (if (= "JJJJJ" hand)
      f
      (let [jokers (get f \J 0)
            best-target (->> (dissoc f \J)
                             (sort-by second)
                             last
                             first
                             )]
        (update (dissoc f \J) best-target #(+ % jokers))))
  ))

(defn rank-from-freqs [f]
    (condp = (count f)
      5          10                 ;; 5 different cards
      4          20                 ;; 1 pair
      3  (condp = (apply max (vals f))
           2     30                 ;; 2 pairs
           3     40)                ;; 3 of a kind
      2   (condp = (apply max (vals f))
           3     50                 ;; full house
           4     60)                ;; four of a kind
      1          70                 ;; five of a kind
      ))

(defn rank-p1
  "The better the hand, the higher the result"
  [hand]
  (rank-from-freqs (frequencies hand)))

(defn rank-p2
  "The better the hand, the higher the result, but with jokers!"
  [hand]
  (rank-from-freqs (joker-freqs hand)))

(defn val-p1
  "The higher the card the higher the result"
  [card]
  (condp = card
    \A 20
    \K 15
    \Q 12
    \J 11
    \T 10
    \9 9
    \8 8
    \7 7
    \6 6
    \5 5
    \4 4
    \3 3
    \2 2))

(defn val-p2
  "The higher the card the higher the result, but Jokers are the worst"
  [card]
  (condp = card
    \A 20
    \K 15
    \Q 12
    \J 1
    \T 10
    \9 9
    \8 8
    \7 7
    \6 6
    \5 5
    \4 4
    \3 3
    \2 2))

(defn hand-comparator [rank-fn val-fn hand1 hand2]
  (let [rc (compare (rank-fn hand1) (rank-fn hand2))]
    (if (= 0 rc)
      (loop [h1 (seq hand1)
             h2 (seq hand2)]
        (if (seq h1)
          (let [vc (compare (val-fn (first h1)) (val-fn (first h2)))]
            (if (= 0 vc)
              (recur (rest h1) (rest h2))
              vc))
          0)
        )
      rc)
    )
  )

(defn winnings [comparator]
  (->> input
       (map #(let [[hand bid] (s/split % #" ")]
               {
                :hand hand
                :bid (parse-long bid)
                }
               ))
       (sort-by :hand comparator)
       (reduce
        (fn [r next]
          (-> r
              (update :sum #(+ % (* (:rank r) (:bid next))))
              (update :rank inc)))
        {:rank 1 :sum 0})
       ))

;; part 1
(winnings (partial hand-comparator rank-p1 val-p1))

;; part 2
(winnings (partial hand-comparator rank-p2 val-p2))