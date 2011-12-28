(require 'clojure.string)
(require 'clojure.java.io)

(ns clojure.test.small-world
  (:use clojure.test))

(defn to-list
  [string]
  (reverse
    (map #(read-string %)
      (remove #(clojure.string/blank? %)
        (reduce 
          (fn[memo f] (cons f memo)) (list) (clojure.string/split string #"\s|\t+")
        )
      )
    )
  )
)


(def coordinates-list
  ; hardcoded filename for now
  (with-open [rdr (clojure.java.io/reader "smallworld.txt")]
    (reverse
      (reduce 
        (fn[memo f] (cons (to-list f) memo)) (list) (line-seq rdr)
      )
    )
  )
)


(defn distance
  [x1 y1 x2 y2]
  ; good 'ol Pythagore !!
  (Math/sqrt (+ (Math/pow (- x2 x1) 2) (Math/pow (- y2 y1) 2)))
)


(defn generate-distances
  [point]
  (reverse
    (reduce 
      (fn[memo f] 
      (cons (list (first f) (distance (nth point 1) (nth point 2) (nth f 1) (nth f 2) )) memo)) 
      (list (first point)) 
      coordinates-list 
    )  
  )
)


(def generate-distancesss
  (reverse
    (reduce
      (fn[memo f] (cons (generate-distances f) memo)) (list) coordinates-list  
    )
  )
)


(println generate-distancesss)


(deftest checks-coordinates-list
  (is (= (list 1 0.0 0.0) (first coordinates-list)))
  (is (= 1 (nth (first coordinates-list) 0)))
  (is (= 0.0 (nth (first coordinates-list) 1)))
  (is (= 0.0 (nth (first coordinates-list) 2)))
  (is (= (list 5 79.99 179.99) (nth coordinates-list 4)))
  (is (= 5 (nth (nth coordinates-list 4) 0)))
  (is (= 79.99 (nth (nth coordinates-list 4) 1)))
  (is (= 179.99 (nth (nth coordinates-list 4) 2)))
  (is (= (list 3 -12.2 12.2) (nth coordinates-list 2)))
  (is (= 3 (nth (nth coordinates-list 2) 0)))
  (is (= -12.2 (nth (nth coordinates-list 2) 1)))
  (is (= 12.2 (nth (nth coordinates-list 2) 2)))
)

(deftest check-distance
  (is (= 2.8284271247461903 (distance 2 2 4 4)))
)

(deftest check-generate-distancesss
  (is (= 5 (count generate-distancesss)))
  (is (= 1 (first (nth generate-distancesss 0))))
  (is (= 3 (first (nth generate-distancesss 2))))
  (is (= 5 (first (nth generate-distancesss 4))))
)


(run-all-tests #"clojure.test.small-world")
