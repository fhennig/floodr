(ns floodr.core
  (:require [lanterna.screen :as s]
            [floodr.util :refer :all]
            [floodr.logic.world :as l]
            [floodr.logic.game :as g]
            [floodr.logic.playing :as p]
            [floodr.logic.solvers :as solver]
            [floodr.lanterna-window :as w])
  (:gen-class))

;;; screen

(def scr (ref nil))

(defn set-scr [s] (dosync (ref-set scr s)))
(defn get-scr [] @scr)

;;; drawing

(defn put-ln [line str]
  (if (or (pos? line) (= 0 line))
    (s/put-string (get-scr) 2 line str)
    (let [[_ h] (s/get-size (get-scr))]
      (s/put-string (get-scr) 2 (+ h line) str))))

(defn draw [world]
  (let [[w h] [(:w world) (:h world)]
        [off-x off-y] [2 2]
        color (fn [n] (l/color world n))]
    (doseq [i (range (* w h))]
      (s/put-string (get-scr) (+ (* 2 (mod i w)) off-x) (+ (quot i w) off-y)
                    "  " {:bg (color i)}))))

(defn stats [game]
  (let [title "floodr"
        help "'h' for help"
        gen (str "generation: " (:generation game))
        cs-left (str "blobs left: " (g/clusters-left game))]
    (put-ln 0 (reduce #(str %1 " - " %2) (list title help gen cs-left)))))

(defn put-title []
  (s/put-string (get-scr) 2 0 "f" {:fg :red})
  (s/put-string (get-scr) 3 0 "l" {:fg :green})
  (s/put-string (get-scr) 4 0 "o" {:fg :blue})
  (s/put-string (get-scr) 5 0 "o" {:fg :cyan})
  (s/put-string (get-scr) 6 0 "d" {:fg :magenta})
  (s/put-string (get-scr) 7 0 "r" {:fg :yellow}))

(defn put-main-window [game]
  (w/all-black (get-scr))
  (stats game)
  (put-title)
  (when-not (nil? game)
    (draw (:world game))))

(defn quit [state]
  (set-vals state [:quit true]))

(defn valid-keys [options]
  (set (keys options)))

(defn do-action [key options action-params]
  (let [action (:action (get options key))]
    (apply action action-params)))

(defn user-action [screen options & action-params]
  (let [key (w/get-valid screen (valid-keys options))]
    (do-action key options action-params)))

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
  (w/put-window screen lines options)
  (s/redraw screen)
  (apply user-action screen options action-params))

(defn choose-amount-of-players [conf]
  (let [options {\1 {:action #(set-vals % [:players 1])
                     :desc "single player mode"}
                 \2 {:action #(set-vals % [:players 2])
                     :desc "two players"}
                 \3 {:action #(set-vals % [:players 3])
                     :desc "etc."}
                 \4 {:action #(set-vals % [:players 4])
                     :desc "..."}}
        order [\1 \2 \3 \4]]
    (w/put-window (get-scr) order options)
    (s/redraw (get-scr))
    (user-action (get-scr) options conf)))

(defn change-neighbor-setup [conf]
  (if (= (:neighbors conf) :4) (set-vals conf [:neighbors :8])
      (set-vals conf [:neighbors :4])))

(defn finish-setup [conf]
  (set-vals conf [:setup-finished true]))

(defn setup-config [scr conf]
  (if (:setup-finished conf) conf
      (let [options {\p {:action choose-amount-of-players
                         :desc "choose amount of players (1 - 4)"}
                     \s {:action change-neighbor-setup
                         :desc "switch between 4 and 8 neighbors"}
                     \n {:action finish-setup
                         :desc "start a new game with this configuration"}}
            window-text ["game configuration" "" 
                         (str "players:   " (:players conf))
                         (str "AIs:       " (:ais conf))
                         (str "neighbors: " (case (:neighbors conf)
                                              :4 "direct"
                                              :8 "direct and diagonal"))
                         "" \p \s "" \n]]
        (w/put-window scr window-text options)
        (s/redraw scr)
        (recur scr (user-action (get-scr) options conf)))))

(defn create-new-game [conf]
  (p/setup-players (g/new-game (l/gen-rand-world (:w conf) (:h conf)
                                                 (:neighbors conf)))
                   (:players conf)
                   (:ais conf)))

(defn new-game [state]
  (let [init-conf (set-vals (:game-conf state) [:setup-finished false])
        conf (setup-config (:screen state) init-conf)]
    (set-vals state
              [:game (create-new-game conf)]
              [:game-conf conf])))

(defn show-winner [state]
  (draw-and-action (:screen state)
                   #(put-main-window (:game state))
                   {\r {:action #(set-vals % [:game (create-new-game (:game-conf %))])
                        :desc "start new game"}
                    \n {:action new-game
                        :desc "configure a new game"}
                    \q {:action quit
                        :desc "quit"}}
                   ["game over" ""
                    (str (p/get-leader (:game state)) " won!") ""
                    \r \n \q]
                   state))

(defn with-close-option [& [options]]
  (let [opts (if options options {})]
    (assoc opts \c {:action no-op
                    :desc "close"})))

(defn show-debug [state]
  (draw-and-action (:screen state)
                   #(put-main-window (:game state))
                   (with-close-option)
                   ["debug window"
                    ""
                    (str "currently winning: " (p/get-leader (:game state)))
                    ""
                    (str "r: " (solver/potential-gain (:game state) :red))
                    (str "g: " (solver/potential-gain (:game state) :green))
                    (str "b: " (solver/potential-gain (:game state) :blue))
                    (str "c: " (solver/potential-gain (:game state) :cyan))
                    (str "m: " (solver/potential-gain (:game state) :magenta))
                    (str "y: " (solver/potential-gain (:game state) :yellow))
                    "" \c]
                   state))

(defn show-help [draw-fn options]
  (draw-fn (with-close-option options)
           ["controls" ""
            \q \n "" \s \d \f \j \k \l "" \c]))

(defn options-with-help [draw-fn options]
  (let [no-op-options (into {} (map (fn [[k v]]
                                      [k (assoc v :action no-op)])
                                    options))]
    (assoc options \h {:action #(do (show-help draw-fn
                                               no-op-options) %)
                       :desc "hidden"})))

(defn p-move
  "lets the current player make his move with the given color
  and lets every AI that has to move after him move"
  [state col]
  (let [ns (update-vals state
                        [:game g/player-move col]
                        [:game p/move-ais])]
    (if (g/finished? (:game ns))
      (show-winner ns)
      ns)))

(defn user-move [s]
  (if (:quit s) s
      (let [draw-fn (fn [& args] (apply draw-and-action (:screen s) #(put-main-window (:game s)) args))]
        (recur (draw-fn (options-with-help draw-fn
                                           {\q {:action quit
                                                :desc "quits the game"}
                                            \n {:action new-game
                                                :desc "start a new game"}
                                            \s {:action #(p-move % :red)
                                                :desc "colorize your blob in red"}
                                            \d {:action #(p-move % :green)
                                                :desc "colorize your blob in green"}
                                            \f {:action #(p-move % :blue)
                                                :desc "colorize your blob in blue"}
                                            \j {:action #(p-move % :cyan)
                                                :desc "colorize your blob in cyan"}
                                            \k {:action #(p-move % :magenta)
                                                :desc "colorize your blob in magenta"}
                                            \l {:action #(p-move % :yellow)
                                                :desc "colorize your blob in yellow"}
                                            \c {:action #(p-move % (solver/greedy-select-col (:game %)))
                                                :desc "hidden"}
                                            \i {:action show-debug
                                                :desc "hidden"}})
                        nil s)))))

;;; init

(defn- init-screen [args]
  (let [scr (if (contains? (set args) "--swing")
              (s/get-screen :swing)
              (s/get-screen :unix))]
    (s/start scr)
    (s/get-key scr) ; HACK to fix size
    (s/redraw scr)
    (set-scr scr) ; TODO remove this line
    scr))

(defn- init-state [args]
  (let [screen (init-screen args)
        [sw sh] (s/get-size screen)
        w (floor (/ (- sw 4) 2))
        h (- sh 3)
        init-game-conf {:w w :h h
                        :players 2
                        :ais 1
                        :neighbors :4}
        i-state {:game (create-new-game init-game-conf)
                 :game-conf init-game-conf
                 :screen screen}]
    i-state))

(defn- destruct-state [state]
  (s/stop (:screen state)))

(defn -main
  "Nothing to see here."
  [& args]
  (destruct-state (user-move (init-state args))))
