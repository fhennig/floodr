(ns floodr.logic.game-test
  (:require [clojure.test :refer :all]
            [floodr.test-data :as t]
            [floodr.logic.game :refer :all]
            [floodr.logic.world :as w]))

(deftest test-new-game
  (new-game t/test-world1))

(deftest test-add-player
  (let [tg1 t/test-game1
        tg2 (add-player tg1)]
    (is (= (+ 1 (count (:players tg1)))
           (count (:players tg2))))))

(deftest test-player-move
  (let [tg1 t/test-game1
        move-color :green
        tg2 (player-move tg1 move-color)]
    (is (not (= (:current-player tg1) (:current-player tg2))))
    (is (= (w/color (:world tg2) (player-cluster tg2 (:current-player tg1))) move-color))))

(deftest test-worst-best-player
  (let [tg1 t/test-game1
        tg2 (player-move tg1 :green)]
    (= 1 (best-player tg1))
    (= 0 (worst-player tg1))
    (= 0 (best-player tg2))
    (= 1 (worst-player tg2))))

(deftest test-finished?
  (is (not (finished? t/test-game1)))
  (is (finished? t/test-game2)))
