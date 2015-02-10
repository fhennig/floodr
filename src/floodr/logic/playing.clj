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
  (if (= player-count 0) (g/set-start-player game)
      (let [human-count (- player-count ai-count)]
        (if (> human-count 0)
          (recur (g/add-player game (new-player (str "Player " (inc (g/player-count game)))
                                                :human))
                 (dec player-count) ai-count)
          (recur (g/add-player game (new-player (str "Computer") :ai))
                 (dec player-count) (dec ai-count))))))

(defn move-ais
  [game]
  (let [c-player (get (:player-info game) (:current-player game))]
    (if (= (:type c-player) :human) game
        (recur (ai/greedy-move game)))))
