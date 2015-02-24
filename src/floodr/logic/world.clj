(ns floodr.logic.world
  (:require [clojure.set :as set]
            [floodr.util :refer :all]))



(defn- parent
  [w n]
  (get (:parents w) n))

(defn cluster 
  "returns the cluster that the node is a member of"
  [world node]
  (let [p (parent world node)]
    (if (= node p) node
      (recur world p))))

(defn nodes->clusters
  [w nodes]
    (set (map #(cluster w %) nodes)))

(defn clusters [w]
  (nodes->clusters w (vals (:parents w))))

(defn nodes [w]
  (set (keys (:parents w))))

(defn same-cluster? [w n1 n2]
  (= (cluster w n1) (cluster w n2)))

(defn- path
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

(defn has-color?
  [w c col]
  (= (color w c) col))

(defn size
  "gets the accumulated size of the given clusters, without duplicates"
  [w & nodes]
  (apply + (map #(get (:sizes w) %) 
                (nodes->clusters w nodes))))

(defn neighbors 
  "returns the neighbor clusters of the cluster"
  [world node]
  (set (map #(cluster world %)
            (get-in world [:neighbors (cluster world node)]))))

(defn- merge-clusters-h
  "returns a new world where c1 and c2 are merged"
  [w n1 n2]
  (let [c1 (cluster w n1), c2 (cluster w n2)]
    (if (= c1 c2) w
        (let [[cs cb] (sort-by #(size w %) [c1 c2])]
          (m-update-in w [[:neighbors] assoc cb (set/union (disj (neighbors w cs) cb)
                                                         (disj (neighbors w cb) cs))]
                       [[:sizes] assoc cb (+ (size w cs) (size w cb))]
                       [[:parents] assoc cs cb]
                       [[:neighbors] dissoc cs]
                       [[:sizes] dissoc cs]
                       [[:colors] dissoc cs])))))

(defn merge-clusters
  "merges any number of clusters"
  [w c1 & cr]
  (reduce #(merge-clusters-h %1 c1 %2) w cr))

;;; world definition

(def colors '(:red :green :blue :yellow :cyan :magenta))

(def empty-world
  {:w 0 :h 0            ; used within the UI
   :parents {}          ; maps nodes to their parents; used to get the cluster of a node
   :neighbors {}        ; what neighbors does a cluster have
   :colors {}           ; what color does a cluster have
   :sizes {}            ; size of the clusters
   :available-slots '() ; nodes that can be owned by players
   :flag nil})          ; flag for CTF

;;; rectangular world generation 

(defn- add-node
  [w node node-ns node-col]
  (m-assoc-in w
            [[:parents node] node]
            [[:neighbors node] node-ns]
            [[:colors node] node-col]
            [[:sizes node] 1]))

(defn- gen-neighbors 
  [w h [x y] neighborhood-fn]
  (set (filter #(in-bounds? w h %)
               (neighborhood-fn x y))))

(defn- neighbors-4
  [x y]
  (list [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]))

(defn- neighbors-8
  [x y]
  (list [(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)] [(dec x) y]
        [(inc x) y] [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]))

(defn- colorize
  [w c]
  (let [col (color w c)]
    (apply merge-clusters w c (filter #(has-color? w % col) (neighbors w c)))))

(defn- possible-slots
  "generates a vector of possible starting positions for the given world"
  [w h]
  (distinct [[0 0]                      ; top left
             [(- w 1) (- h 1)]          ; bot right
             [(- w 1) 0]                ; top right
             [0 (- h 1)]]))             ; bot left

(defn- gen-flag [w h]
  (let [cx (/ w 2)
        cy (/ h 2)]
      [(rand-nth [(ceil cx) (floor cx)])
       (rand-nth [(ceil cy) (floor cy)])]))

(defn- gen-nodes [w h]
  (apply concat (for [y (range 0 h)]
                  (for [x (range 0 w)]
                    [x y]))))

(defn gen-rand-world
  "generates a rectangular world of the given size"
  [width height neighbor-type]
  (let [n-fn (case neighbor-type
               :4 neighbors-4
               :8 neighbors-8)
        add (fn [world node]
              (add-node world node
                        (gen-neighbors width height node n-fn)
                        (rand-nth colors)))
        nodes (gen-nodes width height)
        init-world (m-assoc-in empty-world
                               [[:w] width]
                               [[:h] height]
                               [[:available-slots] (possible-slots width height)]
                               [[:flag] (gen-flag width height)])
        w (reduce add init-world nodes)
        w (reduce #(colorize %1 %2) w nodes)] w))
