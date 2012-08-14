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
; needs to increase heap size on OSX otherwise FIREWORKS OF FAIL
; java -Xms512m -Xmx1g -cp ~/clojure-1.4.0/clojure-1.4.0.jar clojure.main ~/leetcode.clj
; 20 secs for n=8
(defn chessboard
  [n]
  (for [x (range n) y (range n)] [x y]))


(defn add-queen
  [board queen]
  (map 
    (fn[cell] 
      (if (or (= 'Q cell) (= 'X cell))
        cell
        (let [[xcell ycell] cell [xqueen yqueen] queen diffx (- xqueen xcell) diffy (- yqueen ycell)]
          (cond
            (= xcell xqueen) 'X
            (= ycell yqueen) 'X
            (and (> yqueen ycell) (= (+ xqueen diffy) xcell)) 'X
            (and (< yqueen ycell) (= (- xqueen diffy) xcell)) 'X
            (and (> yqueen ycell) (= (- xqueen diffy) xcell)) 'X
            (and (< yqueen ycell) (= (+ xqueen diffy) xcell)) 'X
            :else cell
          )))) (map (fn[cell] (if (= cell queen) 'Q cell)) board)))


(defn next-gen
  [boards]
  (reduce 
    (fn[result board] 
      (into result (map (fn[queen] (add-queen board queen)) (filter #(and (not= % 'Q) (not= % 'X)) board))))
        (set nil) boards))


(defn nqueens
  [n]
  (reduce (fn[result n] (next-gen result)) [(chessboard n)] (range 1 (+ n 1))))


(deftest test-nqueens
  (is
    '(
      (
        X Q X X 
        X X X Q 
        Q X X X 
        X X Q X
      ) 
      (
        X X Q X 
        Q X X X 
        X X X Q 
        X Q X X
      )
    ) (nqueens 4))
    ;http://en.wikipedia.org/wiki/Eight_queens_puzzle#Counting_solutions 
    (is (= 2 (count (nqueens 4))))
    (is (= 10 (count (nqueens 5))))
    (is (= 4 (count (nqueens 6))))
    (is (= 40 (count (nqueens 7))))
    (is (= 92 (count (nqueens 8)))))
; /N Queens


; Next Permutation
; did that already for Project Euler, pretty much copy/paste
(defn find-k
  [coll]
    (loop [k (- (count coll) 2)]
      (if (= -1 k)
        nil
        (if (< (nth coll k) (nth coll (+ k 1)))
          k
          (recur (dec k)) ))))


(defn find-l
  [coll k]
  (if (= nil k)
    nil
    (loop [l (- (count coll) 1)]
      (if (< (nth coll k) (nth coll l))
        l
        (recur (dec l)) ))))


(defn swap
  [coll k l]
    (assoc coll k (nth coll l) l (nth coll k)))


(defn rev
  [coll k]
  (into (subvec coll 0 (+ k 1)) (reverse (subvec coll (+ k 1)))))


(defn lexi-perms
  [start stop]
  (if (= (reverse(sort start)) start) 
    (into [] (sort start))
    (loop [coll start i 1]
      (let [k (find-k coll) l (find-l coll k)]
        (if (or (= nil k) (= stop i))
          coll
          (recur (rev (swap coll k l) k) (inc i))) ))))


(deftest test-next-permutation
  (is (= [1 3 2] (lexi-perms [1 2 3] 2)))
  (is (= [1 2 3] (lexi-perms [3 2 1] 2)))
  (is (= [1 5 1] (lexi-perms [1 1 5] 2))))
; /Next Permutation


; Palindrome Number
(defn palindrome-nb
  [n]
  (let [nb (seq (str n))]
    (= nb (reverse nb))))
  

(deftest test-palindrome-nb
  (is (= true (palindrome-nb 1221)))
  (is (= false (palindrome-nb 1234))))
; /Palindrome Number


; Permutation Sequence
; piggybacking on next-permutation
(deftest test-permutation-sequence
  (let [a (into [] (range 1 4))]
    (= [1 3 2] (lexi-perms a 1))
    (= [2 3 1] (lexi-perms a 3))
    (= [3 2 1] (lexi-perms a 5))))
; /Permutation Sequence



; Permutations
(defn permutations
  [grp]
  
  
  
)
; /Permutations








(run-all-tests #"clojure.test.leetcode")

