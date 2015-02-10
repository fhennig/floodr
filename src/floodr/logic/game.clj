(ns floodr.logic.game
  (:require [floodr.logic.world :refer :all]
            [floodr.util :refer :all]))

(defn new-game [w]
  {:world w
   :generation 0
   :player-info {}
   :player-clusters {}
   :current-player nil})

(defn player-count [g]
  (count (:player-info g)))

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
  [game player-info]
  (let [player-id (player-count game)
        player-cluster (gen-player-node (:world game) player-id)]
    (update-vals game
                 [:player-info assoc player-id player-info]
                 [:player-clusters assoc player-id player-cluster])))

(defn player-cluster
  "return the cluster owned by the given player"
  [g p]
  (cluster (:world g) (get (:player-clusters g) p)))

(defn current-player-cluster
  "return the cluster of the current player"
  [g]
  (player-cluster g (:current-player g)))

(defn player-owned? [g c]
  (let [ps (apply clusters (:world g) (vals (:player-clusters g)))]
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
                 [:current-player #(mod (inc %) (player-count g))])))

(defn- player-h [f g]
  (key (apply f #(size (:world g) (val %)) (:player-clusters g))))

(defn worst-player [g] (player-h min-key g))
(defn best-player [g] (player-h max-key g))

(defn set-start-player [g]
  (set-vals g [:current-player (worst-player g)]))

(defn clusters-left [game]
  (- (count (:clusters (:world game))) (player-count game)))

(defn finished? [game]
  (= 0 (clusters-left game)))

