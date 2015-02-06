(ns floodr.core
  (:require [lanterna.screen :as s])
  (:gen-class))

;;; Constants

(def colors '(:red :blue :green :yellow :cyan :magenta))

(def scr (s/get-screen :swing))

(defn put-random-blocks [screen startx starty width height]
  (doseq [y (range height)]
    (doseq [x (range 0 width 2)]
      (s/put-string screen (+ startx x) (+ starty y)
                    "  " {:bg (rand-nth colors)}))))

(defn quit [screen]
  (s/stop screen)
  (System/exit 0))

(defn foo [x y]
  (+ x y))

(defn handle-input [screen]
  (let [key (s/get-key-blocking screen)]
    (case key
      \q (quit screen)
      \r (put-random-blocks screen 2 2 5 5 )))
  (s/redraw screen)
  (recur screen))

(defn -main
  "Nothing to see here."
  [& args]
  (s/start scr)
  (s/put-string scr 0 0 "Hello World!" {:bg :cyan})
  (put-random-blocks scr 2 2 80 4)
  (s/redraw scr)
  (handle-input scr))
