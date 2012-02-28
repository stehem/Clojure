; http://en.wikipedia.org/wiki/Quicksort

(ns clojure.test.euler
  (:use clojure.test))


; problem 2
(defn fibonacci
  [fib lastt]
  (if (< 4000000 lastt)
    (reduce + (reverse(rest(reverse (filter #(odd? %) fib)))))
    (let [newfib (concat fib (list (+ (first (reverse fib)) (second (reverse fib)))))]
    (recur newfib (last newfib)))))

(println "problem 2 :" (fibonacci (list 0 1) 1))
; / problem 2



; problem 3 WIP
(defn is-prime?
  [n]
  (empty? (filter #(= 0 (rem n %)) (range 2 n))))

(defn primes
  [limit]
  (filter #(not= nil %) (map #(is-prime? %) (range 2 limit))))


(run-all-tests #"clojure.test.euler")
