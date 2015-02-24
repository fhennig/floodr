(ns floodr.logic.game-test
  (:require [clojure.test :refer :all]
            [floodr.test-data :as t]
            [floodr.logic.game :refer :all]
            [floodr.logic.world :as w]))

(deftest test-player-count
  (is (= 2 (player-count t/test-game1)))
  (is (= 2 (player-count t/test-game2)))
  (is (= 0 (player-count t/empty-game))))

(deftest test-clusters-left
  (is (= 0 (clusters-left t/test-game2)))
  (is (= 4 (clusters-left t/test-game1)))
  (is (= 3 (clusters-left (player-move t/test-game1 :green)))))

(defn as-ctf [game]
  (assoc game :mode :ctf))

(deftest test-finished?
  (is (not (finished? t/test-game1)))
  (is (finished? t/test-game2))
  (is (not (finished? (as-ctf t/test-game1))))
  (is (finished? (as-ctf t/test-game2))))

(deftest test-occupied-slots
  (is (= 2 (count (occupied-slots t/test-game1))))
  (is (= 2 (count (occupied-slots t/test-game2))))
  (is (= 0 (count (occupied-slots t/empty-game)))))

(deftest test-occupied?
  (is (occupied? t/test-game1 (first (keys (:slot-occupancy t/test-game1)))))
  (is (not (occupied? t/test-game1 (last (:available-slots t/test-game1))))))

(deftest test-next-active-slot
  (is (= [1 1] (next-active-slot t/test-game2))))

(deftest test-join
  (let [tg1 t/test-game1
        tg2 (join tg1 "Player 3")]
    (is (= (+ 1 (player-count tg1))
           (player-count tg2)))))

(deftest test-leave
  (is (= 1 1)))

(deftest test-player-move
  (let [tg1 t/test-game1
        move-color :green
        tg2 (player-move tg1 move-color)]
    (is (not (= (:active-slot tg1) (:active-slot tg2))))
    (is (= (w/color (:world tg2) (:active-slot tg1)) move-color))))

(deftest test-set-start-slot
  (let [init-game (join (new-game t/test-init-world :flood) "Player 1")]
    (is (not (nil? (:active-slot (set-start-slot init-game)))))))

(deftest test-new-game
  (is (not (nil? (new-game t/test-world1 :flood))))
  (is (not (nil? (new-game t/test-world1 :ctf)))))
