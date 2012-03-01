; http://projecteuler.net/problems
; http://code.google.com/p/projecteuler-solutions/wiki/ProjectEulerSolutions

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

(deftest test-p1
  (is (= 4613732 (fibonacci (list 0 1) 1))))
; / problem 2



; problem 3
(defn is-prime?
  [n]
  (empty? (filter #(= 0 (rem n %)) (range 2 n))))

(defn prime-factor
  [n i]
  (if (and (= 0 (rem n i)) (is-prime? (/ n i)))
    (/ n i)
    (recur n (inc i))))

(println "problem 3 :" (prime-factor 600851475143 2))

(deftest test-p2
  (is (= 6857 (prime-factor 600851475143 2))))
; /problem 3


; problem 4
(defn is-palindrome?
  [n]
  (= (str n) (apply str(reverse(str n)))))

(defn big-pal
  ;it works but needs to be seriously optimized
  [memo n]
  (if (= 100 n)
    (apply max (filter #(is-palindrome? %) (distinct (flatten memo))))
    (recur
      (lazy-cat memo 
        (list(reduce (fn[memo f] (concat(list (* n f))memo)) (list) (range 100 999)))) (dec n))))

(println "problem 4 :" (big-pal (list) 999))

(deftest test-palindrome
  (is (= true (is-palindrome? 9009)))
  (is (= false (is-palindrome? 9018))))

(deftest test-p4
  (is (= 906609 (big-pal (list) 999))))
; /problem 4

(run-all-tests #"clojure.test.euler")
