(ns primesinp
  (:use [clojure.contrib.math :only (gcd)]
        [clojure.contrib.generic.math-functions :only (log pow floor sqrt)]))

; log_2(n)
(defn lg [n]
  (/ (log n)
     (log 2)))

; this function returns true if n = a ^ b for any positive integer a > 1, b > 1.
; it considers 1 to be a perfect power.
(defn perfect-power?
  ([n]
    (if (= n 1)
      true
      (perfect-power? n 2 2)))
  ([n a b]
    (loop [a a
           b b]
      (let [exp (pow a b)]
        (if (> exp n)
          (if (= b 2)
            false
            (recur (inc a) 2))
          (if (= n exp)
            true
            (recur a (inc b))))))))

; Returns the multiplicative order of a modulo n
; http://en.wikipedia.org/wiki/Multiplicative_order
; Given an integer, a, and a positive integer, n, with gcd(a,n) = 1,
;   the multiplicative order of a modulo n is the smallest positive integer k with (a ^ k) mod n = 1.
(defn multiplicative-order [a n]
  (loop [k 1]
    (if (= (mod (pow a k) n)
           1)
      k
      (recur (inc k)))))

; I don't know of a more descriptive name for this function.
; I don't understand at a high level what R represents.
(defn find-r [n]
  (loop [r 1]
    (if (and (= (gcd n r)
                1)
             (> (multiplicative-order n r)
                (pow (lg n) 2)))
      r
      (recur (inc r)))))

; is the following condition true?
; 1 < gcd(n, a) < n for some a and n
(defn gcd-n-a-between-1-n [n a]
  (loop [a 2]
    (let [div (gcd a n)]
      (if (and (< 1 div)
               (< div n))
        true
        (recur (inc a))))))

(defn coprime? [a b]
  (= (gcd a b) 1))

(defn totient [n]
  (count (filter (partial coprime? n)
                 (range 1 (inc n)))))

(defn step-5-condition [n a]
  )

; algorithm taken from http://en.wikipedia.org/wiki/AKS_primality_test
(defn prime? [n]
  (if (perfect-power? n)
    false                                                 ; composite
    (let [r (find-r n)]
      (if (some (partial gcd-n-a-between-1-n n)           ; 1 < gcd(n, a) < n for some a <= r,
                (range 2 (inc r)))
        false                                             ; composite
        (if (<= n r)
          true                                            ; prime
          (if (some (partial step-5-condition n)
                    (range 1 (inc (floor (* (sqrt (totient r))
                                            (lg n))))))
            false                                         ; composite
            true))))))                                    ; prime

; (println (time (perfect-power? 335544320000000)))
; (println (time (multiplicative-order 4 7)))
; (println (time (totient 36)))
