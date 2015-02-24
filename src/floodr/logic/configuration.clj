(ns floodr.logic.configuration
  (:require [floodr.util :refer :all]
            [floodr.logic.game :as g]
            [floodr.logic.world :as l]
            [floodr.logic.playing :as p]))



(defn max-ai-count [conf]
  (dec (:players conf)))

(defn update-player-count [conf pc]
  (let [nc (assoc-in conf [:players] pc) 
        ais (min (:ais conf) (max-ai-count nc))]
    (assoc-in nc [:ais] ais)))

(defn change-neighbor-setup [conf]
  (if (= (:neighbors conf) :4)
    (assoc-in conf [:neighbors] :8)
    (assoc-in conf [:neighbors] :4)))

(defn cycle-game-mode [conf]
  (update-in conf [:game-mode] next-in-cycle g/game-modes))

(defn finish-setup [conf]
  (assoc-in conf [:setup-finished] true))

(defn create-new-game [conf]
  (p/setup-players (g/new-game (l/gen-rand-world (:w conf) (:h conf)
                                                 (:neighbors conf))
                               (:game-mode conf))
                   (:players conf)
                   (:ais conf)))
