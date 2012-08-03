; http://www.leetcode.com/onlinejudge
; did a lot in Ruby, continuing with Clojure


(ns clojure.test.leetcode
  (:use clojure.test))


; multiply strings
(defn multiply-strings
  [s1 s2]
  (str (* (Integer/parseInt s1) (Integer/parseInt s2))))


(deftest test-multiply-strings
  (is (= "4" (multiply-strings "2" "2")))
  (is (= "12" (multiply-strings "3" "4"))))
; /multiply strings


; N Queens
(defn chessboard
  [n]
  (into [] (repeat n (into [] (take n (repeat '.))))))


(defn add-queen
  [queen board]
  (let  [[x y] queen
        x (- x 1) y (- y 1) 
        row (get board y) 
        value (get row x)
        updated-row (if (not= 'Q value) (assoc row x 'Q) row)
        updated-board (assoc board y updated-row)] updated-board))


(defn crosser
  [queen board]
  (let [[x y] queen
        x (- x 1)
        y (- y 1)
        row (get board y)
        horizontal (assoc board x (into [] (map #(if (= % 'Q) % 'X) row)))
        vertical (map (fn[r] (assoc r x (if (not= 'Q (get r x)) 'X (get r x)))) horizontal)
                         
                         
        ]
    vertical

))

;(println (add-queen [8 8] (chessboard 8)))
(println (crosser [8 8] (add-queen [8 8] (chessboard 8))))


(run-all-tests #"clojure.test.leetcode")

