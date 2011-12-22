;java -cp ~/.clojure/clojure.jar clojure.main dancebattle.clj

(require 'clojure.string)
(require 'clojure.java.io)

(ns clojure.test.dance-battle
  (:use clojure.test))


(def number-of-moves
  (with-open [rdr (clojure.java.io/reader "dancebattle.txt")]
    (Integer/parseInt(clojure.string/trim-newline (first (line-seq rdr)))))
)

(def number-of-turns
  (with-open [rdr (clojure.java.io/reader "dancebattle.txt")]
    (Integer/parseInt(clojure.string/trim-newline (second (line-seq rdr)))))
)

(defn map-to-integers
  [f]
  (map #(Integer/parseInt %) (clojure.string/split f #"\s|\t+"))
)

(def past-turns
  (with-open [rdr (clojure.java.io/reader "dancebattle.txt")]
    (reverse
      (reduce 
        (fn[memo,f] (cons (map-to-integers f) memo) ) (list) 
          (filter #(not= nil (re-seq #"\d+\s|\t+\d+" %)) (line-seq rdr))
      )
    )
  )
)

(defn already-danced?
  [turn list]
  (some #(= turn %) list)
)

; all the "other" parameters are future turns

(defn latest-move 
  [other]
  (second (last (concat past-turns other)))
)

; already played includes "virtual" turns, if one turn was 1 0 it also includes 0 1 since
; turns are unordered
(defn already 
  [other]
  (distinct (concat (map #(reverse %) (concat past-turns other)) (concat past-turns other))))

(defn possible
  [other]
  (map #(list (latest-move other) %) (range 0 number-of-moves)))


(defn possible-moves
  [other]
  (remove #(not= nil (already-danced? % (already other)))
    (possible other)
  )
)

(defn is-last-turn?
  [other] 
  (empty? (possible-moves other))
)

; checks a possible turn to see if it gives an end game opportunity to the opponent
(defn will-not-dance-again?
  [other turn]
  (some #(is-last-turn? (list %)) (possible-moves (concat other turn)))
)

; tests

(deftest gets-number-of-moves
  (is (= 3 number-of-moves)))

(deftest gets-number-of-turns
  (is (= 2 number-of-turns)))

(deftest gets-past-turns
  (is (= (list 0 0) (first past-turns)))
  (is (= (list 0 1) (second past-turns))))

(deftest check-already-danced
  ; "some" returns nil instead of false...
  (is (= nil (already-danced? (list 0 2) past-turns)))
  (is (= nil (already-danced? (list 1 1) past-turns)))
  (is (= true (already-danced? (list 0 1) past-turns)))
  (is (= true (already-danced? (list 0 0) past-turns))))

(deftest check-latest-move
  (is (= 1 (latest-move ()))))

(deftest check-possible-moves
  (is (= (list 1 1) (first (possible-moves ()))))
  (is (= (list 1 2) (second (possible-moves ())))))

(deftest check-last-turn
  ; test adding "virtual" turns to the existing .txt stack
  (is (= true (is-last-turn? (list (list 1 2) (list 2 0)))))
  (is (= true (is-last-turn? (list (list 1 1) (list 1 2) (list 2 0)))))
  (is (= false (is-last-turn? (list (list 1 2)))))
  (is (= false (is-last-turn? (list (list 1 1) (list 1 2))))))

(deftest check-wont-dance-again
  (is (= true (will-not-dance-again? () (list(list 1 2)))))
  (is (= true (will-not-dance-again? (list(list 1 1)) (list(list 1 2)))))
  (is (= nil (will-not-dance-again? () (list(list 1 1))))))


(run-all-tests #"clojure.test.dance-battle")
