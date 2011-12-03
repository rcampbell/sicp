(ns sicp.chapter1.section2
  (:use clojure.test))

;;;; Section 1.2 - Procedures and the Processes They Generate
;;;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2

;;; Exercise 1.9

(comment "linear recursive process"

         (defn + [a b]
           (if (= a 0)
             b
             (inc (+ (dec a) b))))
         
         (+ 4 5)
         (inc (+ 3 5))
         (inc (inc (+ 2 5)))
         (inc (inc (inc (+ 1 5))))
         (inc (inc (inc (inc (+ 0 5)))))
         (inc (inc (inc (inc 5))))
         (inc (inc (inc 6)))
         (inc (inc 7))
         (inc 8)
         9)

(comment "linear iterative process"

         (defn + [a b]
           (if (= a 0)
             b
             (+ (dec a) (inc b))))
         
         (+ 4 5)
         (+ 3 6)
         (+ 2 7)
         (+ 1 8)
         (+ 0 9)
         9)


;;; Exercise 1.10

(defn A [x y]
  "Ackermann's function"
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (A (- x 1)
                 (A x (- y 1)))))

(is (=  1024  (A 1 10)))
(is (= 65536  (A 2  4)))
(is (= 65536  (A 3  3)))

(comment (defn f [n] (A 0 n))  "2n"
         (defn g [n] (A 1 n))  "2g(n - 1)"
         (defn h [n] (A 2 n))  "2g(h(n - 1) - 1)")


;; Excercise 1.11

(defn f [n]
  "recursive process"
  (if (< n 3)
    n
    (+      (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(defn f'
  "iterative process"
  ([n]
     (f' n 2 2 1 0))
  ([n i x y z]
     (cond (< n 3) n
           (= n i) x
           :else (recur n
                        (inc i)
                        (+      x
                           (* 2 y)
                           (* 3 z))
                        x
                        y))))


;; Exercise 1.12

(defn pascal [n k]
  (if (or (= k 0) (= k n))
    1
    (+ (pascal (dec n) k)
       (pascal (dec n) (dec k)))))


;; Exercise 1.13


;; Exercise 1.14


