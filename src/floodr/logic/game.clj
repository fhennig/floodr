(ns floodr.logic.game
  (:require [floodr.logic.world :refer :all]
            [floodr.util :refer :all]))

(defn player-count [g]
  (count (:slot-occupancy g)))

(defn clusters-left [game]
  (- (count (:clusters (:world game))) (player-count game)))

(defn finished? [game]
  (= 0 (clusters-left game)))

(defn occupied? [g slot]
  (contains? (:slot-occupancy g) slot))

(defn occupied-slots [g]
  (filter #(occupied? g %) (:available-slots g)))
            
(defn next-free-slot [g]
  (first (filter #(not (occupied? g %))
                 (:available-slots g))))

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
  (let [ps (apply clusters (:world g) (keys (:slot-occupancy g)))]
    (contains? ps (cluster (:world g) c))))

(defn next-active-slot [g]
  (first (filter #(occupied? g %)
                 (rest (drop-up-to (:active-slot g)
                                   (cycle (:available-slots g)))))))

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

(defn- possible-slots [world]
  (map #(coords->index (:w world) %)
       [[0 0] ;top left
        [(- (:w world) 1) (- (:h world) 1)] ; bot right
        [(- (:w world) 1) 0] ; top right
        [0 (- (:h world) 1)]])) ; bot left

(defn new-game [w]
  (let [slots (possible-slots w)]
    {:world w ; should not be changed
     :generation 0
     :available-slots slots ; should not be changed
     :slot-occupancy {}
     :active-slot nil}))

(defn set-start-slot [g]
  (let [s (apply min-key #(size (:world g) %) (occupied-slots g))]
    (assoc-in g [:active-slot] s)))
