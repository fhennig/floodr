(ns floodr.logic.solvers
  (:require [floodr.logic.world :as l]
            [floodr.logic.game :as g]))

(defn- adjacent-colors
  "returns the colors of which there are adjacent clusters"
  [w cluster]
  (distinct (map #(l/color w %) (l/neighbors w cluster))))

(defn potential-gain
  "amout of nodes with color adjacent to the cluster of the current player
  and not already owned by a player"
  [g col]
  (apply l/size (:world g)
         (g/clusters-to-merge g (g/active-slot-cluster g) col)))

(defn- can-move? [g]
  (> (count (filter #(not (g/player-owned? g %))
                    (l/neighbors (:world g) (g/active-slot-cluster g))))
     0))

(defn greedy-select-col [g]
  (if (not (can-move? g)) (rand-nth l/colors)
      (apply max-key #(potential-gain g %) l/colors)))

(defn greedy-move [g]
  (g/player-move g (greedy-select-col g)))
