(ns floodr.logic.world
  (:require [clojure.set :as set]
            [floodr.util :refer :all]))



(defn parent
  [w n]
  (get (:parents w) n))

(defn cluster 
  "returns the cluster that the node is a member of"
  [world node]
  (let [p (parent world node)]
    (if (= node p) node
      (recur world p))))

(defn clusters
  [w & nodes]
    (set (map #(cluster w %) nodes)))

(defn path
  "mainly used for debugging, returns the path that the node takes
  to determine its cluster"
  [w n]
  (let [p (parent w n)]
    (if (= n p) '()
        (cons p (path w p)))))
        
;;; functions that operate on clusters

(defn color
  "gets the color of a cluster"
  [w node]
  (get (:colors w) (cluster w node)))

(defn size
  "gets the accumulated size of the given clusters, without duplicates"
  [w & nodes]
  (apply + (map #(get (:sizes w) %) 
                (apply clusters w nodes))))

(defn neighbors 
  "returns the neighbor clusters of the cluster"
  [world node]
  (set (map #(cluster world %)
            (get (:neighbors world) (cluster world node)))))

(defn player-owned?
  [w c]
  (let [ps (apply clusters w (vals (:players w)))]
    (contains? ps (cluster w c))))

(defn has-color?
  [w c col]
  (= (color w c) col))

(defn mergeable-neighbors
  "returns the clusters that are adjacent and of the same color"
  [w cluster]
  (let [c-col (color w cluster)]
    (filter #(and (not (player-owned? w %))
                  (has-color? w % c-col))
            (neighbors w cluster))))
  
(defn merge-clusters
  "returns a new world where c1 and c2 are merged"
  [w n1 n2]
  (let [c1 (cluster w n1) c2 (cluster w n2)
        cs (if (> (size w c1) (size w c2)) c2 c1) ; smaller cluster
        cb (if (> (size w c1) (size w c2)) c1 c2)] ; bigger cluster
    (update-vals w [:clusters disj cs]
                 [:parents assoc cs cb]
                 [:neighbors assoc cb (set/union (disj (neighbors w cs) cb)
                                                 (disj (neighbors w cb) cs))]
                 [:neighbors dissoc cs]
                 [:colors dissoc cs]
                 [:sizes assoc cb (+ (size w cs) (size w cb))]
                 [:sizes dissoc cs])))

(defn merge-neighbors
  "merges all mergeable neighbors"
  [w c]
  (reduce #(merge-clusters %1 (cluster %1 c) %2)
          w (mergeable-neighbors w c)))

(defn colorize
  "changes the color of the given cluster and merges its neighbors"
  [w clust color]
  (let [c (cluster w clust)
        nw (update-vals w [:colors assoc c color])]
    (merge-neighbors nw c)))

;;; player related functions

(defn- gen-player-node
  [world player]
  (coords->index (:w world)
                 (case player
                   0 [0 0] ; top left
                   1 [(- (:w world) 1) (- (:h world) 1)] ; bot right
                   2 [(- (:w world) 1) 0] ; top right
                   3 [0 (- (:h world) 1)] ; bot left
                   nil)))

(defn add-player
  "adds a player to the given world. up to 4 players supported"
  [world]
  (let [player-id (count (:players world))
        player-cluster (gen-player-node world player-id)]
    (update-vals world [:players assoc player-id player-cluster])))

(defn player-move
  "lets the current player colorize his cluster in the given color"
  [w color]
  (let [player-cluster (get (:players w) (:current-player w))]
    (update-vals (colorize w player-cluster color) [:generation inc]
                 [:current-player #(mod (inc %) (count (:players w)))])))

;;; WORLD STUFF

(def colors '(:red :green :blue :yellow :cyan :magenta))

(def empty-world
  {:w 0 :h 0 ; used within the UI
   :generation 0 ; how many colorize operations were done on this world
   :clusters #{} ; the clusters that exist
   :parents {} ; maps nodes to their parents; used to get the cluster of a node
   :neighbors {} ; what neighbors does a cluster have
   :colors {} ; what color does a cluster have
   :sizes {} ; size of the clusters
   :players {} ; map of player-id to the node of the player
   :current-player 0})

;;; functions that operate on a world

(defn finished? [world]
  (= (count (:clusters world))
     (count (:players world))))

;;; World generation

(defn- add-node ; TODO make private 
  [w node node-ns node-col]
  (update-vals w [:clusters conj node]
               [:parents assoc node node]
               [:neighbors assoc node node-ns]
               [:colors assoc node node-col]
               [:sizes assoc node 1]))

(defn- gen-neighbors 
  [w h i neighborhood-fn]
  (let [[x y] (index->coords w i)]
    (set (map #(coords->index w %)
         (filter #(in-bounds? w h %)
                 (neighborhood-fn x y))))))

(defn- neighbors-4
  [x y]
  (list [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]))

(defn- neighbors-8
  [x y]
  (list [(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)] [(dec x) y]
        [(inc x) y] [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]))


(defn gen-rand-world
  [width height]
  (let [nodes (range (* width height))
        ki (- (* width height) 1)
        init-world (set-vals empty-world
                             [:w width]
                             [:h height])
        add (fn [world node]
              (add-node world node
                        (gen-neighbors width height node neighbors-4)
                        (rand-nth colors)))
        w1 (reduce add init-world nodes)
        w2 (reduce #(colorize %1 %2 (color %1 %2)) w1 nodes)]
    w2))
