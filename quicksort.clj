; http://en.wikipedia.org/wiki/Quicksort

(ns clojure.test.quicksort
  (:use clojure.test))


(defn quicksort
  [lis]
    (if (>= 1 (count lis)) lis
      (concat (quicksort (filter #(> (first lis) %) lis)) 
              (filter #(= (first lis) %) lis) 
              (quicksort (filter #(< (first lis) %) lis)))))


(println (quicksort (list 8 6 7 7 9 5 4 9999 1 2 0 2 123 -12)))

(deftest test-quicksort
  (is (= (list -12 0 1 2 2 4 5 6 7 7 8 9 123 9999) (quicksort (list 8 6 7 7 9 5 4 9999 1 2 0 2 123 -12))))
)

(run-all-tests #"clojure.test.quicksort")
