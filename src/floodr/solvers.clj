(ns floodr.solvers
  (:require [floodr.logic :as l]))



(defn adjacent-colors
  "returns the colors of which there are adjacent clusters"
  [w cluster]
  (distinct (map #(l/color w %) (l/neighbors w cluster))))

(defn potential-gain
  "amout of nodes with color adjacent to the given cluster and not already owned by a player"
  [w cluster col]
  (apply l/size w (filter #(and (l/has-color? w % col)
                                (not (l/player-owned? w %)))
                          (l/neighbors w cluster))))

(defn greedy-step [w cluster]
  (if (l/finished? w) w
      (do (let [color (reduce #(if (> (potential-gain w cluster %2)
                                      (potential-gain w cluster %1)) %2 %1)
                              (adjacent-colors w cluster))]
            (l/colorize w cluster color)))))

(defn greedy-solve [w cluster]
  (if (l/finished? w) w
      (recur (greedy-step w cluster) cluster)))
