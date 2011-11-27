(ns sicp.core)

;;;; Chapter 1

;;; Exercise 1.1
(assert (= 10  10))
(assert (= 12  (+ 5 3 4)))
(assert (=  8  (- 9 1)))
(assert (=  3  (/ 6 2)))
(assert (=  6  (+ (* 2 4) (- 4 6))))
;(assert (= #'sicp.core/a (def a 3))) ; Scheme returns nil, Clojure
                                     ; returns the newly created and
                                     ; interned var
                                        ;(assert (= #'sicp.core/b (def b (+ a 1))))
(def a 3)
(def b (+ a 1))


;;; Exercise 1.2
(assert (= -37/150
           (/ (+ 5
                 4
                 (- 2
                    (- 3
                       (+ 6 4/5))))
              (* 3
                 (- 6 2)
                 (- 2 7)))))

;;; Exercise 1.3
(defn sum-of-squares [a b c]
  (let [larger ((comp (partial take 2) reverse sort) [a b c])]
    (apply + (map * larger larger))))

;;; Exercise 1.4
;;; The if conditional selects which operator to use at runtime based
;;; on the value of b.

;;; Exercise 1.5
;;; If Ben uses an applicative-order interpreter the expression
;;; (test 0 (p)) will evaluate forever, recursively evaluating the
;;; operand (p) to no end. If Ben is uses a normal-order interpreter,
;;; the evaluation will return 0 since (p) will never have to be
;;; evaluated due to the short-circuited if conditional.

;;; Excercise 1.6
;;; Using new-if the sqrt-iter procedure would recurse forever because
;;; new-if is not a special form with special operand evaluation
;;; rules. In other words, new-if will eagerly evaluate its operands -
;;; in this case a recursive call to sqrt-iter - rather than using
;;; lazy evaluation like the primative if special form.

;;; Excercise 1.7
;;; TODO

;;; Excercise 1.8
(letfn [(cube [x] (* x x x))
        (square [x] (* x x))
        (average [x y] (/ (+ x y) 2))
        (improve [guess x] (/ (+ (/ x
                                    (square guess))
                                 (* 2 guess))
                              3))
        (good-enough? [guess x] (< (Math/abs (- (cube guess) x))
                                   0.001))]
  (defn cube-root
    ([x]
       (cube-root 1.0 x))
    ([guess x]
       (if (good-enough? guess x)
         guess
         (recur (improve guess x)
                x)))))










