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
  (apply l/size (:world g) (filter #(and (l/has-color? (:world g) % col)
                                         (not (g/player-owned? g %)))
                                   (l/neighbors (:world g) (g/current-player-cluster g)))))

(defn greedy-move [g]
  (if (g/finished? g) g
      (do (let [color (apply max-key #(potential-gain g %) l/colors)]
            (g/player-move g color)))))
