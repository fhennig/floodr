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
      (let [human-count (- player-count ai-count)
            p-nr (inc (g/player-count game))]
        (if (> human-count 0)
          (recur (g/add-player game (new-player (str "Human (" p-nr ")")
                                                :human))
                 (dec player-count) ai-count)
          (recur (g/add-player game (new-player (str "Computer (" p-nr ")") :ai))
                 (dec player-count) (dec ai-count))))))

(defn get-leader [g]
  (get-in g [:player-info (g/best-player g) :name]))

(defn move-ais
  [game]
  (cond (= (get-in game [:player-info (:current-player game) :type] :human)) game
        (g/finished? game) game
        :else (recur (ai/greedy-move game)))))
