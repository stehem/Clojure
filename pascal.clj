; http://en.wikipedia.org/wiki/Pascal's_triangle


(require 'clojure.string)
(require 'clojure.java.io)

(ns clojure.test.pascal-triangle
  (:use clojure.test))


(def new+ (fnil + 0))

(defn with-indexes
  [mylist]
  (map-indexed list mylist)
)

(defn previous
  [indexedline index]  
  (first (filter #(= (- index 1) (first %)) indexedline))
)

(defn sum
  [indexedline member]
  (new+ (second (previous indexedline (first member))) (second member))
)   

(defn new-line
  [indexedline]
  (->>
    (reduce
      (fn[memo f] (concat (list (sum indexedline f)) memo)) (list) indexedline
    )
    (concat (list (second (last indexedline))))
    (reverse)
  )
)

(defn generate-triangle
  [result i]
  (if (= 0 i)
    (reverse result)
    (recur (concat (list (new-line (with-indexes (last (reverse result))))) result) (dec i)))
)
    
(defn triangle
  [start-value number-of-lines]
  (generate-triangle (list (list start-value)) number-of-lines) 
)

(doseq [line (triangle 1 8)] (println (apply str (repeat (- 9 (count line)) " ")) line))

(deftest test-with-indexes
  (is (= (list (list 0 2) (list 1 4) (list 2 8)) (with-indexes (list 2 4 8))))
)

(deftest test-previous
  (is (= (list 0 1) (previous (list (list 0 1) (list 1 2)) 1)))
  (is (= (list 1 2) (previous (list (list 0 1) (list 1 2) (list 2 3)) 2)))
)

(deftest test-sum
  (is (= 1 (sum (list (list 0 1)) (list 0 1))))
  (is (= 3 (sum (list (list 0 1) (list 1 2)) (list 1 2))))
  (is (= 5 (sum (list (list 0 1) (list 1 2) (list 2 3)) (list 2 3))))
)

(deftest test-new-line
  (is (= (list 1 1) (new-line (with-indexes (list 1)))))
  (is (= (list 1 2 1) (new-line (with-indexes (list 1 1)))))
  (is (= (list 1 3 3 1) (new-line (with-indexes (list 1 2 1)))))
  (is (= (list 1 4 6 4 1) (new-line (with-indexes (list 1 3 3 1)))))
)

(deftest test-this-thing-works
  (is (= (list 2) (first (triangle 2 10))))
  (is (= (list 2 2) (second (triangle 2 10))))
  (is (= (list 2 4 2) (nth (triangle 2 10) 2)))
  (is (= (list 2 6 6 2) (nth (triangle 2 10) 3)))
  (is (= (list 2 8 12 8 2) (nth (triangle 2 10) 4)))
)

(run-all-tests #"clojure.test.pascal-triangle")
