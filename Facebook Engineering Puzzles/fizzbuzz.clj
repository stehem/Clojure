(require 'clojure.string)

(ns clojure.test.example
  (:use clojure.test))


(def get-number #(Integer/parseInt (clojure.string/trim-newline (slurp "fizzbuzz.txt"))))

(defn fizzbuzz
  [number]
  (cond
    (and (= 0 (rem number 3)) (not= 0 (rem number 5))) "fizz"
    (and (= 0 (rem number 5)) (not= 0 (rem number  3))) "buzz"
    (and (= 0 (rem number 5)) (= 0 (rem number 3))) "fizzbuzz"
    :else (println number)
  )
)

(deftest gets-number-from-file
  (is (= 100 (get-number))))

(deftest this-fizzbuzz-thing
  (is (= "fizz" (fizzbuzz 9)))  
  (is (= "fizz" (fizzbuzz 18)))  
  (is (= "fizz" (fizzbuzz 27)))  
  (is (= "buzz" (fizzbuzz 10)))  
  (is (= "buzz" (fizzbuzz 25)))  
  (is (= "buzz" (fizzbuzz 55)))  
  (is (= "fizzbuzz" (fizzbuzz 45)))  
  (is (= "fizzbuzz" (fizzbuzz 75)))  
  (is (= "fizzbuzz" (fizzbuzz 90)))  
)

; the actual fizzbuzz console printout
(doseq [i (range 1 (get-number))] (println (fizzbuzz i)))

(run-all-tests #"clojure.test.example")
