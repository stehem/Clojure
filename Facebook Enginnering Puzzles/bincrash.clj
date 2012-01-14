; http://www.facebook.com/careers/puzzles.php?puzzle_id=2
  

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
    (with-open [rdr (clojure.java.io/reader "bincrash.txt")])
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

(def prices-by-100
  (reduce
    (fn[memo f] (merge {(nth f 1) (* 100 (/ (nth f 2) (nth f 1)))} memo)) {} lines
  ) 
)

(def sorted-prices-by-100
  (into 
    (sorted-map-by (fn [k1 k2] (compare (get prices-by-100 k1) (get prices-by-100 k2)))) prices-by-100
  )  
)

(def sorted-without-above
   (let [m sorted-prices-by-100]
  (select-keys m (for [[k v] m :when (< k weight-to-drop)] k))) 
)

; add the cheapest per 100 while under weight to drop


(defn dump-manifest
  [sum cheapest]
  (if (> (reduce + (concat sum cheapest)) weight-to-drop)
    sum
    (recur (concat sum cheapest) cheapest)
  )    
)



(println weight-price-hash)
(println prices-by-100)
(println sorted-prices-by-100)
(println sorted-without-above)
(println (dump-manifest (list) (list 700)))

; tests

(deftest check-weight-to-drop
  (is (= 1250 weight-to-drop))         
)

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

(deftest check-prices-without-above
  (is (= nil (get sorted-without-above 1300)))         
)

(run-all-tests #"clojure.test.bin-crash")
