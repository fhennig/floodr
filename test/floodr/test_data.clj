(ns floodr.test-data)

(def test-init-world
  {:w 2 :h 2
   :parents {[0 0] [0 0], [1 0] [1 0], [0 1] [0 1], [1 1] [1 1]}
   :colors {[0 0] :red, [1 0] :green, [0 1] :blue, [1 1] :yellow}
   :sizes {[0 0] 1, [1 0] 1, [2 0] 1, [1 1] 1}
   :distances {[0 0] 0, [1 0] 1, [2 0] 1, [1 1] 1}
   :available-slots [[0 0] [1 0] [0 1] [1 1]]
   :flag [0 0]})

(def test-world1
  "typical 3 * 3 world. ([1 0], [0 1], [1 1]) and ([1 2], [2 2]) are clusters"
  {:w 3 :h 3
   :parents {[2 2] [1 2], [1 2] [1 2], [0 2] [0 2], [2 1] [2 1], [1 1] [0 1], [0 1] [1 0], [2 0] [2 0], [1 0] [1 0], [0 0] [0 0]}
   :neighbors {[1 2] #{[1 0] [0 2] [2 1]},
               [0 2] #{[1 2] [0 1]},
               [2 1] #{[1 1] [2 0] [2 2]},
               [2 0] #{[1 0] [2 1]},
               [1 0] #{[0 0] [1 2] [0 2] [2 0] [2 1]},
               [0 0] #{[1 0] [0 1]}}
   :colors {[1 2] :red, [0 2] :yellow, [2 1] :blue, [2 0] :magenta, [1 0] :green, [0 0] :cyan}
   :sizes {[1 2] 2, [0 2] 1, [2 1] 1, [2 0] 1, [1 0] 3, [0 0] 1}
   :distances {[1 2] 1, [0 2] 1.5, [2 1] 1, [2 0] 1.5, [1 0] 0, [0 0] 1.5}
   :available-slots [[0 0] [2 2] [2 0] [0 2]]
   :flag [1 1]})

(def test-nodes1
  #{[0 0] [1 0] [2 0] [0 1] [1 1] [2 1] [0 2] [1 2] [2 2]})

(def test-clusters1
  #{[0 0] [1 0] [2 0] [2 1] [0 2] [1 2]})

(def test-game1
  {:world test-world1
   :mode :flood
   :generation 1
   :slot-occupancy {[0 0] "Player 1", [2 2] "Player 2"}
   :active-slot [0 0]})

(def test-cgame1
  (assoc test-game1 :mode :ctf))

(def test-game2
  {:world {:w 2 :h 2
           :parents {[0 0] [0 0], [1 0] [0 0], [0 1] [1 1], [1 1] [1 1]}
           :neighbors {[0 0] #{[1 1]} [1 1] #{[0 0]}}
           :colors {[0 0] :red [1 1] :green}
           :sizes {[0 0] 2 [1 1] 2}
           :available-slots [[0 0] [1 0] [0 1] [1 1]]
           :flag [0 0]}
   :mode :flood
   :generation 3
   :slot-occupancy {[0 0] "Player 1", [1 1] "Player 2"}
   :active-slot [0 0]})

(def empty-game
  {:world test-world1
   :mode :flood
   :generation 0
   :slot-occupancy {}
   :active-slot nil})
