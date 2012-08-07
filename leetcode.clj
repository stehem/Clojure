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
        row (get board y) 
        value (get row x)
        updated-row (if (not= 'Q value) (assoc row x 'Q) row)
        updated-board (assoc board y updated-row)] updated-board))


(defn cross-horizontal
  [queen board]
  (let [[x y] queen
        row (get board y)]
    (assoc board y (into [] (map #(if (= % 'Q) % 'X) row)))))


(defn cross-vertical
  [queen board]
  (let [[x y] queen]
  (map (fn[r] (assoc r x (if (not= 'Q (get r x)) 'X (get r x)))) board)))


(defn cross-diagonal
  [queen board]
  (let [[x y] queen]
    (map-indexed 
      (fn[irow row] 
        (into [] (map-indexed (fn[icell cell] 
          (let [diffx (- x icell) diffy (- y irow)]
            (if 
              (or 
                (and (> y irow) (= (+ x diffy) icell)) 
                (and (< y irow) (= (- x diffy) icell))
                (and (> y irow) (= (- x diffy) icell)) 
                (and (< y irow) (= (+ x diffy) icell))
              )
            'X cell)
          )
        )row)))board)))


(defn crosser
  [queen board]
  (let [horizontal (cross-horizontal queen board)
        vertical (cross-vertical queen horizontal)
        diagonal (cross-diagonal queen vertical)
        updated-board diagonal]
    (into [] updated-board)))


(defn nqueens
  [n]
  (let [board (chessboard n)]
    
  ))


;(println (add-queen [8 8] (chessboard 8)))
;(println (crosser [8 8] (add-queen [8 8] (chessboard 8))))

(let [onequeen (crosser [0 0] (add-queen [0 0] (chessboard 4)))
      twoqueen (crosser [2 1] (add-queen [2 1] onequeen))]
(doseq [x onequeen]
  ;(println x)
  ))


(doseq [x (nqueens 4)] (println x))

(run-all-tests #"clojure.test.leetcode")

