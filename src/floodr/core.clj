(ns floodr.core
  (:require [lanterna.screen :as s]
            [floodr.logic.world :as l]
            [floodr.logic.solvers :as solver]
            [floodr.lanterna-window :as w])
  (:gen-class))

;;; util

(defn floor [x]
  (int (Math/floor x)))

;;; Constants

(def user-cluster 0)

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

(defn stats [world]
  (let [title "floodr"
        help "'h' for help"
        gen (str "generation: " (:generation world))
        cs-left (str "blobs left: " (- (count (:clusters world)) 1))]
    (put-ln 0 (reduce #(str %1 " - " %2) (list title help gen cs-left)))))

(defn win []
  (w/show-window (get-scr)
                 ["good job!"
                  " "
                  "[q - quit] [s - new game]"]
                 {:centered true}))

(defn redraw [world]
  (all-black)
  (stats world)
  (draw world)
  (when (l/finished? world) (win))
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

(defn new-world []
  (let [[w h] (s/get-size (get-scr))
        w1 (l/gen-rand-world (floor (/ (- w 4) 2))
                                (- h 3))
        w2 (l/add-player w1)
        w3 (l/add-player w2)]
    w3))

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

(defn show-debug [w]
  (w/show-window (get-scr)
                 ["debug window"
                  ""
                  (str "r: " (solver/potential-gain w user-cluster :red))
                  (str "g: " (solver/potential-gain w user-cluster :green))
                  (str "b: " (solver/potential-gain w user-cluster :blue))
                  (str "y: " (solver/potential-gain w user-cluster :yellow))
                  (str "c: " (solver/potential-gain w user-cluster :cyan))
                  (str "v: " (solver/potential-gain w user-cluster :magenta))])
  (s/get-key-blocking (get-scr)))

(defn handle-input [world]
  (redraw world)
  (let [key (s/get-key-blocking (get-scr))]
    (case key
      \q (recur (quit))
      \n (recur (new-world))
      \h (do (show-help) (recur world))
      \? (do (show-help) (recur world))
      \d (do (show-debug world) (recur world))
      \k (recur (solver/greedy-move world))
      \r (recur (l/player-move world :red))
      \g (recur (l/player-move world :green))
      \b (recur (l/player-move world :blue))
      \y (recur (l/player-move world :yellow))
      \c (recur (l/player-move world :cyan))
      \v (recur (l/player-move world :magenta)) ; violet
      (recur world))))



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
  (redraw (new-world))
  (let [w (new-world)]
    (handle-input w)))
