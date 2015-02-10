(ns floodr.core
  (:require [lanterna.screen :as s]
            [floodr.logic.world :as l]
            [floodr.logic.game :as g]
            [floodr.logic.playing :as p]
            [floodr.logic.solvers :as solver]
            [floodr.lanterna-window :as w])
  (:gen-class))

;;; util ; TODO move to floodr.util

(defn floor [x]
  (int (Math/floor x)))

;;; screen

(def scr (ref nil))

(defn set-scr [s] (dosync (ref-set scr s)))
(defn get-scr [] @scr)

;;; drawing

(defn all-black []
  (let [[w h] (s/get-size (get-scr))]
    (doseq [y (range h)]
      (doseq [x (range w)]
        (s/put-string (get-scr) x y " ")))))

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

(defn show-winner [game]
  (w/show-window (get-scr)
                 ["game over"
                  ""
                  (str "player " (g/best-player game) " won!")
                  ""
                  "[q - quit] [n - new game]"]
                 {:centered true}))

(defn redraw [game]
  (all-black)
  (stats game)
  (draw (:world game))
  (when (g/finished? game) (show-winner game))
  (s/redraw (get-scr)))
    
    

(defn quit []
  (s/stop (get-scr))
  (System/exit 0))

;(def supported-players #{1 2 4})

;(defn config-world [config]
;  (w/show-window (get-scr)
;                 ["start a new game"
;                  ""
;                  (str "players: " (:players config) "[p<N>] to change, where <N> = 1, 2 or 4")
;                  (str "how many of these are AI: " (:ais config) "[a<N>] to change")
;                  ""
;                  "press [n] to start-game"])
;  (case (w/get-valid (get-scr) #{\n \p \a})
;    \n config
;    \p (let [ps (w/get-digit (get-scr))]
;         (if (contains? supported-players ps)
;           (recur (l/set-vals config [:players ps])) (recur config)))
;    \a (let [ais (w/get-digit (get-scr))]
;         (if (contains? supported-players ais)
;           (recur (l/set-vals config [:ais ais])) (recur config)))
;    (recur config)))

(defn new-game []
  (let [[w h] (s/get-size (get-scr))
        world (l/gen-rand-world (floor (/ (- w 4) 2))
                                (- h 3))]
    (p/setup-players (g/new-game world) 2 1)))


(defn show-help []
  (w/show-window (get-scr)
                 ["controls:"
                  " "
                  "  q - quit"
                  "  h - show this help"
                  "  n - start new game"
                  " "
                  "  r - red"
                  "  g - green"
                  "  b - blue"
                  "  c - cyan"
                  "  v - violet"
                  "  y - yellow"
                  " " " "
                  "press any key to continue"])
  (s/get-key-blocking (get-scr)))

(defn show-debug [game]
  (w/show-window (get-scr)
                 ["debug window"
                  ""
                  (str (g/worst-player game))
                  (str (:current-player game))
                  (str "r: " (solver/potential-gain game :red)) ; FIXME
                  (str "g: " (solver/potential-gain game :green))
                  (str "b: " (solver/potential-gain game :blue))
                  (str "y: " (solver/potential-gain game :yellow))
                  (str "c: " (solver/potential-gain game :cyan))
                  (str "v: " (solver/potential-gain game :magenta))])
  (s/get-key-blocking (get-scr)))

(defn handle-input [g]
  (let [game (p/move-ais g)]
    (redraw game)
    (let [key (s/get-key-blocking (get-scr))]
      (case key
        \q (recur (quit))
        \n (recur (new-game))
        \h (do (show-help) (recur game))
        \? (do (show-help) (recur game))
        \d (do (show-debug game) (recur game))
        \k (recur (solver/greedy-move game))
        \r (recur (g/player-move game :red))
        \g (recur (g/player-move game :green))
        \b (recur (g/player-move game :blue))
        \y (recur (g/player-move game :yellow))
        \c (recur (g/player-move game :cyan))
        \v (recur (g/player-move game :magenta)) ; violet
        (recur game)))))



(defn handle-args
  "handles command line parameters"
  [args]
  (let [as (set args)]
    (if (contains? as "--swing")
      (set-scr (s/get-screen :swing))
      (set-scr (s/get-screen :unix)))))



(defn -main
  "Nothing to see here."
  [& args]
  (handle-args args)
  (s/start (get-scr))
  (s/get-key (get-scr)) ; workaround for tiling managers
  (redraw (new-game)) ; FIXME
  (let [w (new-game)]
    (handle-input w)))
