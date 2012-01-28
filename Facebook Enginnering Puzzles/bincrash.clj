; http://www.facebook.com/careers/puzzles.php?puzzle_id=2
; how this works, it will use the cheapest per 100 until it would exceed the target, then 
; it will look for the cheapest way to bridge the last gap by using "individual sums" of each block
; it does not compute combinations between blocks for now i.e. 700 + 700 and not 200 + 75 :-(
 
  

(require 'clojure.string)
(require 'clojure.java.io)

(ns clojure.test.bin-crash
  (:use clojure.test))


(defn to-list
  [string]
  (->> 
    (reduce 
      (fn[memo f] (cons f memo)) (list) (clojure.string/split string #"\s|\t+")
    )   
    (remove #(clojure.string/blank? %))
    (map #(read-string %))
    (reverse)
  )
)


(def lines
  (->>
    (reduce 
      (fn[memo f] (cons (to-list f) memo)) (list) (rest (line-seq rdr))
    )
    (reverse)
    (with-open [rdr (clojure.java.io/reader "test.in")])
  )  
)


(def weight-to-drop
  (with-open [rdr (clojure.java.io/reader "test.in")]
    (Integer/parseInt(clojure.string/trim-newline (first (line-seq rdr))))
  )
)


(def weight-price-hash
  (reduce
    (fn[memo f] (merge {(nth f 1) (nth f 2)} memo)) {} lines
  )
)


(def prices-by-100
  (reduce
    (fn[memo f] (merge {(nth f 1) (* 100 (/ (nth f 2) (nth f 1)))} memo)) {}
    (filter #(> weight-to-drop (second %)) lines) 
  ) 
)

(defn sorter
  [sortme]  
  (into 
    (sorted-map-by (fn [k1 k2] (compare (get sortme k1) (get sortme k2)))) sortme
  )
)

(def sorted-prices
  (sorter weight-price-hash)
)

(def sorted-prices-by-100
  (sorter prices-by-100)
)

(def cheapest-by-100 
  (get (first sorted-prices-by-100) 0)
)

(defn remaining
  [sum]  
  (let [rem (- weight-to-drop (reduce + sum))]
    (reduce 
      (fn[memo f] 
        (merge {(get f 0) (* (Math/ceil (/ rem (get f 0))) (get f 1))} memo))
       {} sorted-prices
    )  
  )
)

(defn sorted-remaining
  [sum]
  (sorter (remaining sum))
)

(defn map-prices 
  [sum]
  (reduce 
    (fn[memo f] (concat (list (get weight-price-hash f)) memo)) (list) sum
  )
)

(defn dump-manifest
  [sum cheapest]
  (if (<= weight-to-drop (get (first sorted-prices) 0))
    (map-prices (list (get (first sorted-prices) 0)))
  (if (>= (reduce + (concat sum cheapest)) weight-to-drop)
    (concat (map-prices sum) (list (get (first (sorted-remaining sum)) 1)))
    (recur (concat sum cheapest) cheapest)
  ))
)

(def dump-manifest-price
  (reduce + (dump-manifest (list) (list cheapest-by-100)))
)



(println dump-manifest-price)

; tests


(deftest check-weight-price-hash
  (is (= 3250 (get weight-price-hash 200)))
  (is (= 10500 (get weight-price-hash 1300)))
  (is (= 4750 (get weight-price-hash 700)))
  (is (= 10250 (get weight-price-hash 75)))
)

(deftest check-prices-by-100
  (is (= 1625 (get prices-by-100 200)))
)

(deftest check-sorted-prices-by-100
  (is (= [700 4750/7] (first sorted-prices-by-100)))
  (is (= [75 41000/3] (last sorted-prices-by-100)))
)


(deftest check-remaining
  (is (= 82000 (get (remaining (list 700)) 75)))
  (is (= 4750 (get (remaining (list 700)) 700)))
  (is (= 9750 (get (remaining (list 700)) 200)))
  (is (= 10500 (get (remaining (list 700)) 1300)))
)

(deftest this-thing-work
  (is (= 9500 dump-manifest-price))
)

(run-all-tests #"clojure.test.bin-crash")
