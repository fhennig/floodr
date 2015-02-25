(ns floodr.cli.core
  (:require [floodr.cli.output :as o]
            [floodr.cli.input :as i]
            [floodr.util :refer :all]
            [floodr.logic.configuration :as c]
            [floodr.logic.world :as w]
            [floodr.logic.game :as g]
            [floodr.logic.playing :as p]
            [floodr.logic.solvers :as solver])
  (:gen-class))

;;; user interaction 

(defn choose-amount-of-players [choose-opt conf]
  (choose-opt {\1 {:action #(c/update-player-count % 1)
                   :desc "single player mode"}
               \2 {:action #(c/update-player-count % 2)
                   :desc "two players"}
               \3 {:action #(c/update-player-count % 3)
                   :desc "etc."}
               \4 {:action #(c/update-player-count % 4)
                   :desc "..."}}
              [\1 \2 \3 \4]
              conf))

(defn build-ai-opts [max-count]
  (let [opts (for [i (range 0 (inc max-count))
                   :let [opt [(digit->char i)
                              {:action #(assoc-in % [:ais] i)
                               :desc (str i (if (= 1 i) " AI" " AIs"))}]]] opt)
        options (into {} opts)
        text (map first opts)]
    [options text]))

(defn choose-ai-count [choose-opt conf]
  (if (= (:players conf) 1) conf ; there has to be at least one human
      (let [[opts text] (build-ai-opts (c/max-ai-count conf))]
        (choose-opt opts text conf))))

(defn conf-desc [conf]
  [(str "game mode: " (case (:game-mode conf)
                        :flood "flood"
                        :ctf "capture the flag"))
   (str "players:   " (:players conf))
   (str "AIs:       " (:ais conf))
   (str "neighbors: " (case (:neighbors conf)
                        :4 "direct"
                        :8 "direct and diagonal"))])

(defn setup-config [choose-opt conf]
  (choose-opt {\m {:action c/cycle-game-mode
                   :desc "choose game mode (flood, CTF)"}
               \p {:action #(choose-amount-of-players choose-opt %)
                   :desc "choose amount of players (1 - 4)"}
               \a {:action #(choose-ai-count choose-opt %)
                   :desc (str "choose amount of AIs (0 - " (c/max-ai-count conf) ")")}
               \s {:action c/change-neighbor-setup
                   :desc "switch between 4 and 8 neighbors"}
               \n {:action c/finish-setup
                   :desc "start a new game with this configuration"}}
              (concat ["game configuration" ""]
                      (conf-desc conf)
                      ["" \m \p \a \s "" \n])
              conf))

(defn new-game [choose-opt state]
  (let [ns (assoc-in state [:game-conf :setup-finished] false)
        conf (loop [conf (:game-conf ns)]
               (if (:setup-finished conf) conf
                   (recur (setup-config choose-opt conf))))]
    (m-assoc-in state
                [[:game] (c/create-new-game conf)]
                [[:game-conf] conf])))

(defn quit [state]
  (assoc-in state [:quit] true))

(defn format-player
  "formats a player into a string"
  [{:keys [name id]}]
  (str name " (" id ")"))

(defn show-winner [choose-opt state]
  (choose-opt {\r {:action #(assoc-in % [:game] (c/create-new-game (:game-conf %)))
                :desc "start new game"}
            \n {:action #(new-game choose-opt %)
                :desc "configure a new game"}
            \q {:action quit
                :desc "quit"}}
           ["game over" ""
            (str (format-player (g/leader (:game state))) " won!") ""
            \r \n \q]
           state))

(defn with-close-option [& [options]]
  (let [opts (if options options {})]
    (assoc opts \c {:action no-op
                    :desc "close"})))

(defn show-debug [choose-opt {:keys [game] :as state}]
  (choose-opt (with-close-option)
              ["debug window"
               ""
               (str "flag: " (get-in game [:world :flag]))
               (str "dist: " (w/dist (:world game) (:active-slot game)))
               (str "clusters left: " (g/clusters-left game))
               (str "currently winning: " (format-player (g/leader game)))
               ""
               (str "r: " (solver/potential-gain game :red))
               (str "g: " (solver/potential-gain game :green))
               (str "b: " (solver/potential-gain game :blue))
               (str "c: " (solver/potential-gain game :cyan))
               (str "m: " (solver/potential-gain game :magenta))
               (str "y: " (solver/potential-gain game :yellow))
               "" \c]
              state))

(defn show-help [choose-opt options order]
  (choose-opt (with-close-option options)
              (into [] (concat ["help" ""] order ["" \c]))))

(defn options-with-help [choose-opt options order]
  (let [no-op-options (into {} (map (fn [[k v]]
                                      [k (assoc v :action no-op)])
                                    options))]
    (assoc options \? {:action #(do (show-help choose-opt
                                               no-op-options
                                               order) %)
                       :desc "hidden"})))

(defn p-move
  "lets the current player make his move with the given color
  and lets every AI that has to move after him move"
  [state col]
  (m-update-in state [[:game] g/player-move col]))

(defn user-move [choose-opt s]
  (cond (g/finished? (:game s)) (show-winner choose-opt s)
        (= :ai (get-in s [:game :slot-occupancy (:active-slot (:game s)) :type])) (do ;(Thread/sleep 10)
                                                                                      (update-in s [:game] solver/greedy-move))
        :else (choose-opt (options-with-help choose-opt
                                   {\q {:action quit
                                        :desc "quits the game"}
                                    \n {:action #(new-game choose-opt %)
                                        :desc "start a new game"}
                                    \s {:action #(p-move % :red)
                                        :desc "colorize your block in red"}
                                    \d {:action #(p-move % :green)
                                        :desc "colorize your block in green"}
                                    \f {:action #(p-move % :blue)
                                        :desc "colorize your block in blue"}
                                    \j {:action #(p-move % :cyan)
                                        :desc "colorize your block in cyan"}
                                    \k {:action #(p-move % :magenta)
                                        :desc "colorize your block in magenta"}
                                    \l {:action #(p-move % :yellow)
                                        :desc "colorize your block in yellow"}
                                    \c {:action #(p-move % (solver/greedy-select-col (:game %)))
                                        :desc "hidden"}
                                    \i {:action #(show-debug choose-opt %)
                                        :desc "hidden"}}
                                   ["controls" "" \q \n "" \s \d \f \j \k \l ""
                                    "you can use the 'floodr' text at the top as an orientation,"
                                    "it is colored in the same order as the keys are on the keyboard."])
             nil s)))

(defn stats
  "returns a vector of strings with information about the game"
  [{:keys [generation slot-occupancy active-slot] :as game}]
  (if (= (g/player-count game) 1)
    [(str "generation: " generation)]
    [(str "next move: " (format-player (slot-occupancy active-slot)))
     (str "currently winning: " (format-player (g/leader game)))]))

;;; drawing

(defn block-at
  "returns a vector representing the block at the given coordinates"
  [{:keys [world active-slot mode]} node dot]
  [node (w/color world node)
   (cond (and (= mode :ctf) (= (:flag world) node)) "##"
         (and dot (w/same-cluster? world node active-slot)) ".")])

(defn world->blocks
  "extracts blocks in the form of [x y :color text] from the world"
  [{:keys [world active-slot] :as game}]
  (let [[w h] [(:w world) (:h world)]
        players-with-same-color (filter #(= (w/color world active-slot)
                                            (w/color world %))
                                        (g/non-active-slots game))
        dot (not (empty? (clojure.set/intersection (w/nodes->clusters world players-with-same-color)
                                                   (w/neighbors world active-slot))))]
    (map #(block-at game % dot) (w/nodes world))))

(defn pp [x] (println x) x)
  
(defn put-main-window [screen game]
  (o/all-black screen)
  (o/put-stats screen (stats game))
  (o/put-title screen)
  (o/put-blocks screen (world->blocks game)))

(defn valid-keys [options]
  (set (keys options)))

(defn do-action [key options action-params]
  (let [action (:action (get options key))]
    (apply action action-params)))

(defn draw-and-action
  "takes:
  - a screen to draw on and to read input from
  - a function to clear the screen
  - a map with keys to actions
  - optionally lines of text that should be displayed to the user
  - optional parameters to the actions definied earlier
  returns:
  - the result of applying the optional parameters to one of the actions"
  [screen clear-fn options & [lines & action-params]]
  (clear-fn)
  (o/put-window screen lines options)
  (o/redraw screen)
  (let [choice (i/get-valid screen (valid-keys options))]
    (do-action choice options action-params)))

;;; init
  
(defn- init-state [screen]
  (let [[sw sh] (o/get-size screen)
        w (floor (/ (- sw 4) 2))
        h (- sh 3)
        init-game-conf {:w w :h h
                        :players 2
                        :ais 1
                        :neighbors :4
                        :game-mode :flood}
        i-state {:game (c/create-new-game init-game-conf)
                 :game-conf init-game-conf}]
    i-state))

(defn run [screen]
  (loop [s (init-state screen)]
    (put-main-window screen (:game s))
    (o/redraw screen)
    (let [choose-opt (fn [& args] (apply draw-and-action screen
                                      #(put-main-window screen (:game s)) args))]
      (if (:quit s) s
          (recur (user-move choose-opt s)))))
  screen)

(defn- init-screen [args]
  (o/init-screen (if (contains? (set args) "--swing")
                   :swing :unix)))

(defn -main
  "Nothing to see here."
  [& args]
  (o/desctruct-screen (run (init-screen args))))
