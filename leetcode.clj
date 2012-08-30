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
      (if (not (vector? cell))
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
      (into result (map (fn[queen] (add-queen board queen)) (filter #(vector? %) board))))
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
    (is (= 92 (count (nqueens 8))))
         )
; /N Queens


; Next Permutation
; did that already for Project Euler, pretty much copy/paste
; OLD CODE, PREFER REDUCE OVER LOOP RECUR => MORE IDIOMATIC
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



; Permutations 1&2
(defn permutations
  [grp]
  (loop [i 2 perm (lexi-perms grp i) perms [grp perm]]
    (if (= (last perms) (last (butlast perms)))
      (butlast perms)
        (let [ii (inc i) next-perm (lexi-perms grp ii)]
          (recur ii next-perm (conj perms next-perm))))))

(deftest test-permutations
  (is '([1 1 2] [1 2 1] [2 1 1]) (permutations [1 1 2]))
  (is '([1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1]) (permutations [1 2 3])))
; /Permutations 1&2


;homemade combination algorithm, way more fun than using clojure.contrib :-)
(defn combine
  [base n]
  (reduce 
    (fn[acc x]  
      (into acc (map (fn[y] (conj x y)) (range (inc (last x)) n))))
    [] base))


(defn map-indices-to-value
  [combs grp]
    (map (fn[comb] (map (fn[idx] (get grp idx)) comb)) combs))


(defn combinations
  [grp c]
  (let [n (count grp) start (map (fn[i] [i]) (range 0 n))]
    (map-indices-to-value
      (reduce (fn[acc x] (combine acc n)) start (range 1 c))
      grp)))


(deftest test-combinations 
  (is (=
       '(
         (a b c) (a b d) (a b e) (a b f) (a b g) (a b h) (a c d) (a c e) (a c f) (a c g) 
         (a c h) (a d e) (a d f) (a d g) (a d h) (a e f) (a e g) (a e h) (a f g) (a f h) 
         (a g h) (b c d) (b c e) (b c f) (b c g) (b c h) (b d e) (b d f) (b d g) (b d h) 
         (b e f) (b e g) (b e h) (b f g) (b f h) (b g h) (c d e) (c d f) (c d g) (c d h) 
         (c e f) (c e g) (c e h) (c f g) (c f h) (c g h) (d e f) (d e g) (d e h) (d f g) 
         (d f h) (d g h) (e f g) (e f h) (e g h) (f g h) 
        ) (combinations '[a b c d e f g h]  3)))
  (is (= 28 (count (combinations '[a b c d e f g h]  2))))
  (is (= 70 (count (combinations '[a b c d e f g h]  4))))
  (is (= 56 (count (combinations '[a b c d e f g h]  5))))
  (is (= 28 (count (combinations '[a b c d e f g h]  6))))
  (is (= 8 (count (combinations '[a b c d e f g h]  7)))))
  ;about 6 secs for generating 3,5M combinations there is room for improvement also INTO IS COSTLY!!!
  ;(time (combinations '[A B C D E F G H I J K L M N O P Q R S T U V W X Y Z]  9)) 
; /homemade combination algorithm


;Subsets
(defn subsets
  [grp]
  (reduce (fn[acc x] (apply conj acc (combinations grp x))) ['()] (range 1 (inc (count grp)))))


(deftest test-subsets
  (is (= '[() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3)] (subsets [1 2 3]))))
; /Subsets


; Rotate Image
; 90 deg clockwise
(defn img-with-index
  [image]
  (reduce (fn[acc row] (into acc (map-indexed (fn[idx itm] [idx itm]) row))) [] image))


(defn rotate-line
  [i img]
  (reduce (fn[acc p] (if (= i (first p)) (conj acc (last p)) acc)) [] img))


(defn rotate-image
  [image]
  (reduce (fn[acc col] (conj acc (rotate-line col (img-with-index image)) )) [] (range 0 (count (first image)))))


(def image
  '[
    [+ + + * + + +]
    [+ + + * * + +]
    [+ + + * * * +]
    [+ + + * * * *]
    [+ + + * * * +]
    [+ + + * * + +]
    [+ + + * + + +]
   ])


(def rotated-image
  '[
    [+ + + + + + +]
    [+ + + + + + +]
    [+ + + + + + +]
    [* * * * * * *]
    [+ * * * * * +]
    [+ + * * * + +]
    [+ + + * + + +]
   ])


(deftest test-rotate-image
  (is (= rotated-image (rotate-image image))))
; /Rotate Image


; Spiral Matrix
(defn flat-with-index
  [m]
  (reduce 
    (fn[acc x] (into acc x)) [] 
      (map-indexed (fn[i e] (map-indexed (fn[ii ee] [i ii]) e)) m)))


(defn spiral-path
  [step stairs res]
  (let [[x y] step]
  (letfn [
    (not-already [step res] (nil? (some #(= step %) res)))
    (next-step [step stairs pred] (first (filter pred stairs)))
    (right [step stairs] (next-step step stairs #(= [x (inc y)] %) ))
    (down [step stairs] (next-step step stairs #(and (< x (first %)) (= y (last %))))) 
    (left [step stairs] (next-step step stairs #(= [x (dec y)] %)))
    (up [step stairs] (next-step step stairs #(= [(dec x) y] %))) ]
      (let [r (right step stairs) d (down step stairs) l (left step stairs) u (up step stairs)]
        (cond
          (and (and r (not-already r res)) (and u (not-already u res))) u
          (and r (not-already r res)) r
          (and d (not-already d res)) d
          (and l (not-already l res)) l
          (and u (not-already u res)) u )))))


(defn map-back-to-n
  [acc m]
  (map (fn[step] (let [[x y] step] (get (get m x) y))) acc))


(defn spiral-matrix
  [m]
  (map-back-to-n (butlast 
    (let [stairs (flat-with-index m)]
      (reduce 
        (fn[acc x] 
          (let [step (last acc)]
            (conj acc (spiral-path step stairs acc))))
        [(first stairs)]
        stairs) )) m))


(def spiral-a
  '[
    [ 1 2 3 ]
    [ 4 5 6 ]
    [ 7 8 9 ]
  ])


(def spiral-b
  '[
    [ 1 2 3 4 ]
    [ 5 6 7 8 ]
    [ 9 10 11 12 ]
    [ 13 14 15 16 ]
  ])


(def spiral-c
  '[
    [ 1 2 3 4 5 ]
    [ 6 7 8 9 10 ]
    [ 11 12 13 14 15 ]
    [ 16 17 18 19 20 ]
    [ 21 22 23 24 25 ]
  ]) 


(deftest test-spiral-matrix
  (is (= '(1 2 3 6 9 8 7 4 5) (spiral-matrix spiral-a)))
  (is (= '(1 2 3 4 8 12 16 15 14 13 9 5 6 7 11 10) (spiral-matrix spiral-b)))
  (is (= '(1 2 3 4 5 10 15 20 25 24 23 22 21 16 11 6 7 8 9 14 19 18 17 12 13) (spiral-matrix spiral-c))))
; /Spiral Matrix



(run-all-tests #"clojure.test.leetcode")

