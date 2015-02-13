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

(defn put-ln [screen line str]
  (if (or (pos? line) (= 0 line))
    (s/put-string screen 2 line str)
    (let [[_ h] (s/get-size screen)]
      (s/put-string screen 2 (+ h line) str))))

(defn draw [screen world]
  (let [[w h] [(:w world) (:h world)]
        [off-x off-y] [2 2]
        color (fn [n] (l/color world n))]
    (doseq [i (range (* w h))]
      (s/put-string screen (+ (* 2 (mod i w)) off-x) (+ (quot i w) off-y)
                    "  " {:bg (color i)}))))

(defn stats [screen game]
  (let [title "floodr"
        help "'h' for help"
        gen (str "generation: " (:generation game))
        cs-left (str "blobs left: " (g/clusters-left game))]
    (put-ln screen 0 (reduce #(str %1 " - " %2) (list title help gen cs-left)))))

(defn put-title [screen]
  (s/put-string screen 2 0 "f" {:fg :red})
  (s/put-string screen 3 0 "l" {:fg :green})
  (s/put-string screen 4 0 "o" {:fg :blue})
  (s/put-string screen 5 0 "o" {:fg :cyan})
  (s/put-string screen 6 0 "d" {:fg :magenta})
  (s/put-string screen 7 0 "r" {:fg :yellow}))

(defn put-main-window [screen game]
  (w/all-black screen)
  (stats screen game)
  (put-title screen)
  (when-not (nil? game)
    (draw screen (:world game))))

(defn quit [state]
  (assoc-in state [:quit] true))

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

;;; user interaction 

(defn max-ai-count [conf]
  (dec (:players conf)))

(defn update-player-count [conf pc]
  (let [nc (assoc-in conf [:players] pc) 
        ais (min (:ais conf) (max-ai-count nc))]
    (assoc-in nc [:ais] ais)))

(defn choose-amount-of-players [choose-opt conf]
  (choose-opt {\1 {:action #(update-player-count % 1)
                :desc "single player mode"}
            \2 {:action #(update-player-count % 2)
                :desc "two players"}
            \3 {:action #(update-player-count % 3)
                :desc "etc."}
            \4 {:action #(update-player-count % 4)
                :desc "..."}}
           [\1 \2 \3 \4]
           conf))

(defn build-ai-opts [max-count]
  (let [opts (for [i (range 1 (inc max-count))
                   :let [opt [(digit->char i)
                              {:action #(assoc-in % [:ais] i)
                               :desc (str i (if (= 1 i) " AI" " AIs"))}]]] opt)
        options (into {} opts)
        text (map first opts)]
    [options text]))

(defn choose-ai-count [choose-opt conf]
  (if (= (:players conf) 1) conf ; there has to be at least one human
      (let [[opts text] (build-ai-opts (max-ai-count conf))]
        (choose-opt opts text conf))))
        

(defn change-neighbor-setup [conf]
  (if (= (:neighbors conf) :4)
    (assoc-in conf [:neighbors] :8)
    (assoc-in conf [:neighbors] :4)))

(defn finish-setup [conf]
  (assoc-in conf [:setup-finished] true))

(defn setup-config [choose-opt conf]
  (choose-opt  {\p {:action #(choose-amount-of-players choose-opt %)
                 :desc "choose amount of players (1 - 4)"}
             \a {:action #(choose-ai-count choose-opt %)
                 :desc (str "choose amount of AIs (0 - " (max-ai-count conf) ")")}
             \s {:action change-neighbor-setup
                 :desc "switch between 4 and 8 neighbors"}
             \n {:action finish-setup
                 :desc "start a new game with this configuration"}}
            ["game configuration" "" 
             (str "players:   " (:players conf))
             (str "AIs:       " (:ais conf))
             (str "neighbors: " (case (:neighbors conf)
                                  :4 "direct"
                                  :8 "direct and diagonal"))
             "" \p \a \s "" \n]
            conf))

(defn create-new-game [conf]
  (p/setup-players (g/new-game (l/gen-rand-world (:w conf) (:h conf)
                                                 (:neighbors conf)))
                   (:players conf)
                   (:ais conf)))

(defn new-game [choose-opt state]
  (let [ns (assoc-in state [:game-conf :setup-finished] false)
        conf (loop [conf (:game-conf ns)]
               (if (:setup-finished conf) conf
                   (recur (setup-config choose-opt conf))))]
    (m-assoc-in state
                [[:game] (create-new-game conf)]
                [[:game-conf] conf])))

(defn show-winner [choose-opt state]
  (choose-opt {\r {:action #(assoc-in % [:game] (create-new-game (:game-conf %)))
                :desc "start new game"}
            \n {:action #(new-game choose-opt %)
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

(defn show-debug [choose-opt state]
  (choose-opt (with-close-option)
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

(defn show-help [choose-opt options]
  (choose-opt (with-close-option options)
           ["controls" ""
            \q \n "" \s \d \f \j \k \l "" \c]))

(defn options-with-help [choose-opt options]
  (let [no-op-options (into {} (map (fn [[k v]]
                                      [k (assoc v :action no-op)])
                                    options))]
    (assoc options \h {:action #(do (show-help choose-opt
                                               no-op-options) %)
                       :desc "hidden"})))

(defn p-move
  "lets the current player make his move with the given color
  and lets every AI that has to move after him move"
  [state col]
  (m-update-in state
               [[:game] g/player-move col]
               [[:game] p/move-ais]))

(defn user-move [choose-opt s]
  (if (g/finished? (:game s))
    (show-winner choose-opt s)
    (choose-opt (options-with-help choose-opt
                                {\q {:action quit
                                     :desc "quits the game"}
                                 \n {:action #(new-game choose-opt %)
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
                                 \i {:action #(show-debug choose-opt %)
                                     :desc "hidden"}})
             nil s)))

;;; init
  
(defn- init-state [screen]
  (let [[sw sh] (s/get-size screen)
        w (floor (/ (- sw 4) 2))
        h (- sh 3)
        init-game-conf {:w w :h h
                        :players 2
                        :ais 1
                        :neighbors :4}
        i-state {:game (create-new-game init-game-conf)
                 :game-conf init-game-conf}]
    i-state))

(defn run [screen]
  (loop [s (init-state screen)]
    (let [choose-opt (fn [& args] (apply draw-and-action screen
                                      #(put-main-window screen (:game s)) args))]
      (if (:quit s) s
          (recur (user-move choose-opt s)))))
  screen)

(defn- init-screen [args]
  (let [scr (if (contains? (set args) "--swing")
              (s/get-screen :swing)
              (s/get-screen :unix))]
    (s/start scr)
    (s/get-key scr) ; HACK to fix size
    (s/redraw scr)
    (set-scr scr) ; TODO remove this line
    scr))

(defn- desctruct-screen [screen]
  (s/stop screen))

(defn -main
  "Nothing to see here."
  [& args]
  (desctruct-screen (run (init-screen args))))
