(ns euler
(:use clojure.contrib.lazy-seqs clojure.contrib.math)
(:use clojure.contrib.combinatorics)
  (:require [clojure.string :as str]))

;; Problem 1: Find the sum of all the multiples of 3 or 5 below 1000.
(defn euler1 [] (reduce +
  (filter #(or
    (= 0 (mod % 3))
    (= 0 (mod % 5)))
  (range 1000))
  )
)

;; Problem 2: Find the sum of all the even-valued terms in fibonachi sequence which do not exceed four million.
(defn euler2 [] (reduce + (filter even? (take-while #(< % 4000000) (fibs)))))

;; Problem 7: What is the 10001^(st) prime number?
(defn euler7 [] (last (take 10001 primes)))

;; Problem 5: What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
(def even-numbers (iterate #(+ 2 %) 2))
(defn divisible-by-first-twenty [x] (every? #(zero? (rem x %)) (range 2 21)))
(defn divisible-by-first-ten [x] (every? #(zero? (rem x %)) (range 2 11)))

(defn euler5 [] (time (first (filter #(divisible-by-first-twenty %) (iterate inc 1)))))

;; Problem 6: Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
(defn square [x] (* x x))
(defn square-of-sum [x] (square (reduce + (range 1 (inc x)))))
(defn sum-of-square [x] (reduce + (map square (range 1 (inc x)))))
(defn euler6 ([] (euler6 100))
  ([x] (- (square-of-sum x) (sum-of-square x))))

;; Problem 20: Find the sum of the digits in the number 100!
(defn fac [x] (loop [val x
                     accum 1]
  (if (= 1 val) accum (recur (dec val) (* accum val)))))

(defn digits [x] (map #(Integer. (str %)) (str x)))
(defn euler20 [] (reduce + (digits (fac 100))))

;; Problem 16
(defn euler16 [] (reduce + (digits (.pow (BigInteger/valueOf 2) 1000))))

;; Problem 3: What is the largest prime factor of the number 600851475143 ?
(defn smallest-factor [x] (first (filter #(zero? (rem x %)) (range 2 (inc (sqrt x))))))

(defn euler3
  ([] (euler3 600851475143))
  ([x] (loop [leftover x]
  (let [min-factor (smallest-factor leftover)]
    (if min-factor (recur (/ leftover min-factor)) leftover)))))

;; Problem 10: Find the sum of all the primes below two million.
(defn euler10 [] (reduce + (take-while #(< % 2000000) primes)))

;; Problem 13
;; (reduce + (map #(BigInteger. %) (read-lines "data/euler13")))

;; Problem 4: Find the largest palindrome made from the product of two 3-digit numbers.
(defn is-palindrome [num] (= (str num) (str/reverse (str num))))

(defn euler4 []
  (first (filter is-palindrome
    (reverse (sort (map #(apply * %) (combinations (range 100 1000) 2)))))))

;; Problem 6: Find the greatest product of five consecutive digits in the 1000-digit number.

(def long-number 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)
(defn euler6 []
  (let
    [digit-list (digits long-number)
     all-five-digit-lists (filter #(= 5 (count %)) (map #(take 5 (drop % digit-list)) (range (count digit-list))))]
      (apply max (map #(apply * %) all-five-digit-lists))))
