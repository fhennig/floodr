(ns floodr.logic.playing
  (:require [floodr.logic.game :as g]
            [floodr.logic.solvers :as ai]))

(defn new-player
  "creates a new player.
  name should be a string, type is either :human or :ai"
  [name type]
  {:name name
   :type type})

(defn setup-players
  "adds the given amount of players to the game and sets the start player"
  [game player-count ai-count]
  (if (= player-count 0) (g/set-start-slot game)
      (let [human-count (- player-count ai-count)
            p-nr (inc (g/player-count game))]
        (if (> human-count 0)
          (recur (g/join game (new-player (str "Human (" p-nr ")")
                                                             :human))
                 (dec player-count) ai-count)
          (recur (g/join game (new-player (str "Computer (" p-nr ")") :ai))
                 (dec player-count) (dec ai-count))))))

(defn leader [g]
  (get-in g [:slot-occupancy (apply max-key #(floodr.logic.world/size (:world g) %) (g/occupied-slots g)) :name]))

(defn move-ais
  [game]
  (cond (= (get-in game [:slot-occupancy (:active-slot game) :type]) :human) game
        (g/finished? game) game
        :else (recur (ai/greedy-move game))))
