(require 'clojure.string)
(require 'clojure.java.io)

(ns clojure.test.dance-battle
  (:use clojure.test))


(defn number-of-moves
  []
  (with-open [rdr (clojure.java.io/reader "dancebattle.txt")]
    (Integer/parseInt(clojure.string/trim-newline (first (line-seq rdr)))))
)

(defn number-of-turns
  []
  (with-open [rdr (clojure.java.io/reader "dancebattle.txt")]
    (Integer/parseInt(clojure.string/trim-newline (second (line-seq rdr)))))
)

(defn map-to-integers
  [f]
  (map (fn [x] (Integer/parseInt x)) (clojure.string/split f #"\s|\t+"))
)

(defn past-turns
  []
  (with-open [rdr (clojure.java.io/reader "dancebattle.txt")]
    (reverse
      (reduce 
        (fn[memo,f] (cons (map-to-integers f) memo) ) (list) 
          (filter (fn[x](not= nil (re-seq #"\d+\s|\t+\d+" x))) (line-seq rdr))
      )
    )
  )
)

(defn already-danced?
  [turn list]
  (some (fn[x] (= turn x)) list)
)


(defn latest-move
  []
  (second (last (past-turns)))
)


(defn possible-moves
  []
  ;doesnt work
  (remove (fn[x] (= nil (already-danced? x (past-turns))))
  (map (fn[x] (list (latest-move) x)) (range 0 (number-of-moves))))
)

(println (possible-moves))

; tests

(deftest gets-number-of-moves
  (is (= 3 (number-of-moves))))

(deftest gets-number-of-turns
  (is (= 2 (number-of-turns))))

(deftest gets-past-turns
  (is (= (list 0 0) (first (past-turns))))
  (is (= (list 0 1) (second (past-turns)))))

(deftest check-already-danced
  ; some returns nil instead of false...
  (is (= nil (already-danced? (list 0 2) (past-turns))))
  (is (= true (already-danced? (list 0 1) (past-turns)))))

(deftest check-latest-move
  (is (= 1 (latest-move))))

(run-all-tests #"clojure.test.dance-battle")
