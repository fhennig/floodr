(ns floodr.logic.game
  (:require [floodr.logic.world :refer :all]
            [floodr.util :refer :all]))

(defn new-game [w]
  {:world w
   :generation 0
   :players {}
   :current-player 0})

(defn- gen-player-node [world player]
  (coords->index (:w world)
                 (case player
                   0 [0 0] ; top left
                   1 [(- (:w world) 1) (- (:h world) 1)] ; bot right
                   2 [(- (:w world) 1) 0] ; top right
                   3 [0 (- (:h world) 1)] ; bot left
                   nil)))

(defn add-player
  "adds a player to the given world. up to 4 players supported"
  [game]
  (let [player-id (count (:players game))
        player-cluster (gen-player-node (:world game) player-id)]
    (update-vals game [:players assoc player-id player-cluster])))

(defn player-cluster
  "return the cluster owned by the given player"
  [g p]
  (cluster (:world g) (get (:players g) p)))

(defn current-player-cluster
  "return the cluster of the current player"
  [g]
  (player-cluster g (:current-player g)))

(defn player-owned? [g c]
  (let [ps (apply clusters (:world g) (vals (:players g)))]
    (contains? ps (cluster (:world g) c))))

(defn player-move
  "lets the current player colorize his cluster in the given color"
  [g color]
  (let [player-cluster (current-player-cluster g)
        clusters-to-merge (filter #(and (not (player-owned? g %))
                                       (has-color? (:world g) % color))
                                 (neighbors (:world g) player-cluster))
        nw (update-vals (apply merge-clusters (:world g) player-cluster clusters-to-merge)
                        [:colors assoc player-cluster color])]
    (update-vals g [:world #(update-vals (apply merge-clusters % player-cluster clusters-to-merge)
                                         [:colors assoc player-cluster color])]
                 [:generation inc]
                 [:current-player #(mod (inc %) (count (:players g)))])))

(defn- player-h [f g]
  (key (apply f #(size (:world g) (val %)) (:players g))))

(defn worst-player [g] (player-h min-key g))
(defn best-player [g] (player-h max-key g))

(defn set-start-player [g]
  (set-vals g [:current-player (worst-player g)]))

(defn finished? [game]
  (= (count (:clusters (:world game)))
     (count (:players game))))

