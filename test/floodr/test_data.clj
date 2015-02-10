(ns floodr.test-data)

(def test-world1
  "typical 3 * 3 world. (1, 3, 4) and (7, 8) are clusters"
  {:w 3 :h 3
   :clusters #{0 1 2 5 6 7}
   :parents {8 7, 7 7, 6 6, 5 5, 4 3, 3 1, 2 2, 1 1, 0 0}
   :neighbors {7 #{1 6 5}, 6 #{7 3}, 5 #{4 2 8}, 2 #{1 5}, 1 #{0 7 6 2 5}, 0 #{1 3}}
   :colors {7 :red, 6 :yellow, 5 :blue, 2 :magenta, 1 :green, 0 :cyan}
   :sizes {7 2, 6 1, 5 1, 2 1, 1 3, 0 1}})

(def test-nodes1
  #{0 1 2 3 4 5 6 7 8})

(def test-game1
  {:world test-world1
   :generation 1
   :players {0 0, 1 8}
   :current-player 0})

(def test-game2
  {:world {:w 2 :h 2
           :clusters #{0 3}
           :parents {0 0, 1 0, 2 3, 3 3}
           :neighbors {0 #{}}
           :colors {0 :red}
           :sizes {0 4}}
   :generation 3
   :players {0 0, 1 3}
   :current-player 0})
