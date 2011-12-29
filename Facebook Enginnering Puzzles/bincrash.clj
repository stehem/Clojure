; http://www.facebook.com/careers/puzzles.php?puzzle_id=2
  

(require 'clojure.string)
(require 'clojure.java.io)

(ns clojure.test.bin-crash
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


(def lines
  ; hardcoded filename for now
  (with-open [rdr (clojure.java.io/reader "bincrash.txt")]
    (reverse
      (reduce 
        (fn[memo f] (cons (to-list f) memo)) (list) (rest (line-seq rdr))
      )
    )
  )
)


(def weight-to-drop
  (with-open [rdr (clojure.java.io/reader "bincrash.txt")]
    (Integer/parseInt(clojure.string/trim-newline (first (line-seq rdr))))
  )
)


(def weight-price-hash
  (reduce
    (fn[memo f] (merge {(nth f 1) (nth f 2)} memo)) {} lines
  )
)



(println weight-price-hash)
(println (get weight-price-hash 75))


(deftest check-weight-to-drop
  (is (= 1250 weight-to-drop))         
)


(deftest check-weight-price-hash
  (is (= 3250 (get weight-price-hash 200)))         
  (is (= 10500 (get weight-price-hash 1300)))         
  (is (= 4750 (get weight-price-hash 700)))         
  (is (= 10250 (get weight-price-hash 75)))         
)

(run-all-tests #"clojure.test.bin-crash")
