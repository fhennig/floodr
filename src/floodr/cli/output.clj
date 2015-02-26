(ns floodr.cli.output
  (:require [lanterna.screen :as s]
            [floodr.util :refer :all]))

;;; initializing and destructing

(defn init-screen [type]
  (let [scr (s/get-screen type)]
    (s/start scr)
    (s/get-key scr) ; HACK to fix size
    (s/redraw scr)
    scr))

(defn desctruct-screen [screen]
  (s/stop screen))

;;; direct rebinds only to encapsulate

(def get-size s/get-size)
(def redraw s/redraw)

;;; clearing the screen

(defn all-black [screen]
  (let [[w h] (s/get-size screen)]
    (doseq [y (range h)]
      (doseq [x (range w)]
        (s/put-string screen x y " ")))))

;;; padding 

(defn- space [n] (apply str (repeat n " ")))

(defn- spaced-str [& strs]
  (reduce #(str %1 " " %2) strs))

(defn- pad [s len alignment]
  (let [pad-len (- len (count s))
        pad-f (case alignment
                :c (floor (/ pad-len 2))
                :r pad-len
                0)
        pad-b (- pad-len pad-f)]
    (str (space pad-f) s (space pad-b))))

(defn- max-width [words]
  (apply max (map count words)))

(defn- pad-words [words & [alignment]]
  (let [new-width (max-width words)]
    (mapv #(pad % new-width alignment) words)))

;;; format table

(defn table
  "takes a matrix of strings and optionally a seq of alignments
  (:c :l :r), one per column.
  returns a seq of strings (formatted lines of the table)"
  [rows & [alignments]]
  (map #(apply spaced-str %) (transpose (map pad-words
                                      (transpose rows) alignments))))

;;; drawing windows

(defn- put-lines [screen width lines]
  (let [height (count lines)
        [w h] (s/get-size screen)
        [b-l b-b b-t b-r] [3 1 1 3]
        box-w (+ b-l b-r width)
        box-h (+ b-b b-t height)
        empty-ln (space width)
        x (floor (- (/ w 2) (/ box-w 2)))
        y (floor (- (/ h 2) (/ box-h 2)))
        put-ln (fn [line string] 
                 (s/put-string screen x line (str (space b-l) string (space b-r))))]
    (doseq [i (range y (+ y b-t))]
      (put-ln i empty-ln))
    (doseq [i (range (+ y b-t) (+ y b-t height))]
      (put-ln i (nth lines (- i y b-t))))
    (doseq [i (range (+ y b-t height) (+ y b-t height b-b))]
      (put-ln i empty-ln))))

(defn- line->string [line key-descs]
  (cond (string? line) line
        (char? line) (str line " - " (:desc (get key-descs line)))))

(defn put-window [screen & [lines key-descs]]
  "displays a window containing the given lines
   a line may be a single character if a key-description is provided"
  (when-not (nil? lines)
    (let [ls (map #(line->string % key-descs) lines)
          ls-padded (pad-words ls)
          width (count (first ls-padded))]
      (put-lines screen width ls-padded))))

;;; drawing the top status line

(defn put-stats
  "prints the given strings to the top right of the window, separated by ' - '"
  [screen stats]
  (let [statsstr (reduce #(str %1 " - " %2) stats)
        [w _] (s/get-size screen)
        startpos (- w (count statsstr) 2)]
    (s/put-string screen startpos 0 statsstr)))

;;; drawing the game world

(defn- game-coords->screen-coords [x y]
  [(+ 2 (* 2 x)) (+ 2 y)])

(defn- block-str [text]
  (apply str (take 2 (str text "  "))))

(defn- put-block [screen [x y] col & [text]]
  (let [[sx sy] (game-coords->screen-coords x y)]
    (s/put-string screen sx sy (block-str text) {:bg col})))

(defn put-blocks
  "draws a sequence of blocks to the screen, a block is [x y :color],
  where x and y are game coordinates"
  [screen blocks]
  (doseq [b blocks]
    (apply put-block screen b)))

;;; drawing the title

(defn put-title [screen]
  (s/put-string screen 2 0 "f" {:fg :red})
  (s/put-string screen 3 0 "l" {:fg :green})
  (s/put-string screen 4 0 "o" {:fg :blue})
  (s/put-string screen 5 0 "o" {:fg :cyan})
  (s/put-string screen 6 0 "d" {:fg :magenta})
  (s/put-string screen 7 0 "r" {:fg :yellow})
  (s/put-string screen 8 0 " - '?' for help"))
