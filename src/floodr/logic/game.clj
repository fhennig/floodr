(ns floodr.logic.game
  (:require [floodr.logic.world :refer :all]
            [floodr.util :refer :all]))

(defn player-count [g]
  (count (:slot-occupancy g)))

(defn clusters-left [game]
  (- (count (clusters (:world game))) (player-count game)))

(defn occupied? [g slot]
  (contains? (:slot-occupancy g) slot))

(defn occupied-slots
  "returns the occupied slots in the game, in the same order as the available slots are"
  [{:keys [world] :as g}]
  (filter #(occupied? g %) (:available-slots world)))
            
(defn next-free-slot [{:keys [world] :as g}]
  (first (filter #(not (occupied? g %))
                 (:available-slots world))))

(defn active-slot-cluster [g]
  (cluster (:world g) (:active-slot g)))

(defn non-active-slots [g]
  (remove #(= (:active-slot g) %)
          (occupied-slots g)))

(defn owner [g n]
  (let [c (cluster (:world g) n)]
    (get (:slot-occupancy g) (first (filter #(= c (cluster (:world g) %))
                                            (keys (:slot-occupancy g)))))))

(defn player-owned? [g c]
  (let [ps (nodes->clusters (:world g) (keys (:slot-occupancy g)))]
    (contains? ps (cluster (:world g) c))))

(defn next-active-slot [g]
  (next-in-cycle (:active-slot g) (occupied-slots g)))

(defn join 
  "lets a player join the game, optionally at a specified slot"
  [game player & [slot]]
   (assoc-in game [:slot-occupancy (if slot slot (next-free-slot game))] player))

(defn leave [game slot]
  (let [g (dissoc-in game [:slot-occupancy slot])]
    (if (= slot (:active-slot game))
      (assoc g :active-slot (next-active-slot))
      g)))

(defn clusters-to-merge [g clust col]
  (filter #(and (not (player-owned? g %))
                (has-color? (:world g) % col))
          (neighbors (:world g) clust)))

(defn player-move
  "lets the current player colorize his cluster in the given color"
  [g color]
  (let [player-cluster (active-slot-cluster g)
        g2 (m-assoc-in g [[:world :colors player-cluster] color]
                         [[:active-slot] (next-active-slot g)])]
    (m-update-in g2 [[:world] #(apply merge-clusters % player-cluster
                                      (clusters-to-merge g player-cluster color))]
                    [[:generation] inc])))

;;; game generation

(defn new-game [w t]
  {:world w ; should not be changed
   :mode t
   :generation 0
   :slot-occupancy {}
   :active-slot nil})

;;; game rules

(def rank-fns
  {:flood size
   :ctf #(- (dist %1 %2))})

(def game-modes (sort (keys rank-fns)))

(defn rank
  "ranks the given slot in the given game.
  the higher the rank the better the player"
  [game slot]
  (((:mode game) rank-fns) (:world game) slot))

(defmulti finished? :mode)

(defmethod finished? :flood [game]
  (= 0 (clusters-left game)))

(defmethod finished? :ctf [game]
  (player-owned? game (get-in game [:world :flag])))

(defn leader [g]
  (get-in g [:slot-occupancy (apply max-key #(rank g %) (occupied-slots g))]))

(defn set-start-slot [g]
  (let [s (apply min-key #(rank g %) (occupied-slots g))]
    (assoc-in g [:active-slot] s)))
